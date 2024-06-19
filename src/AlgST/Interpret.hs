{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module AlgST.Interpret
  ( -- * Evaluating
    EvalM,
    etaEvalM,
    runEval,
    runEvalWith,
    eval,
    evalName,
    Env,
    programEnvironment,

    -- ** Values
    Value (..),
    pattern Pair,
    ChannelId,
    Channel (channelId, channelSide),
    Side (..),
    BuildClosure,
    buildClosure,

    -- * Evaluation settings
    Settings (..),
    defaultSettings,

    -- * Errors
    InterpretError (..),
  )
where

import AlgST.Builtins (builtinsModule)
import AlgST.Builtins.Names
import AlgST.Driver.Output
import AlgST.Syntax.Decl
import AlgST.Syntax.Expression qualified as E
import AlgST.Syntax.Kind qualified as K
import AlgST.Syntax.Module
import AlgST.Syntax.Name
import AlgST.Typing.Phase (Tc, TcBind, TcExp, TcExpX (..), TcModule, TcStage)
import AlgST.Util.Lenses
import AlgST.Util.SourceLocation
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Control.Monad.Eta
import Control.Monad.Fix
import Control.Monad.Reader
import Control.Monad.State.Strict hiding (evalState)
import Data.DList qualified as DL
import Data.Foldable
import Data.Hashable
import Data.IORef
import Data.List qualified as List
import Data.Map.Lazy qualified as LMap
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Monoid
import Data.Void
import GHC.Stack
import Lens.Family2
import Numeric.Natural (Natural)
import Prettyprinter qualified as P
import Prettyprinter.Render.Terminal qualified as P
import System.IO
import Prelude hiding (log)

-- | The environment associates names with either an unevaluated expression or
-- a final value.
--
-- Unevaluted expressions are used for top-level definitions. They are not
-- updated after evaluation.
--
-- FIXME: Caching of global values seems reasonable.
type Env = Map.Map (ProgVar TcStage) (Either TcExp Value)

-- | A list of spawned threads.
type ThreadList = [Async ()]

-- | A @ThreadName@ gives a stable and reproducible name to a thread,
-- regardless of interleaving. This is achieved by having a list of identifiers
-- which locates the thread in the tree of all spawned threads.
data ThreadName = ThreadName [Word] ShowS

instance Eq ThreadName where
  ThreadName w1 _ == ThreadName w2 _ = w1 == w2

instance Hashable ThreadName where
  hashWithSalt s (ThreadName ws _) = hashWithSalt s ws

instance Show ThreadName where
  showsPrec _ (ThreadName _ s) = s

deriveChildName :: ThreadName -> ForkCounter -> ThreadName
deriveChildName (ThreadName ws ss) (ForkCounter w) =
  ThreadName (w : ws) (ss . showChar '-' . shows (w + 1))

newtype ForkCounter = ForkCounter Word
  deriving newtype (Num)

data Settings = Settings
  { evalDebugMessages :: !Bool,
    evalBufferSize :: !Natural,
    evalOutputHandle :: !OutputHandle
  }

defaultSettings :: Settings
defaultSettings =
  Settings
    { evalDebugMessages = False,
      evalBufferSize = 0,
      evalOutputHandle = nullHandle
    }

data EvalInfo = EvalInfo
  { evalEnv :: !Env,
    evalState :: !(IORef EvalSt),
    evalSettings :: !Settings,
    evalThreadName :: ThreadName
  }

data EvalSt = EvalSt
  { -- | The next channel id to be used.
    stNextChannel :: !ChannelId,
    -- | The list of threads forked during evaluation. Accumulated to be able
    -- to wait for all to complete or to cancel them.
    --
    -- TODO: Think about exception propagation.
    stForked :: ThreadList
  }

newtype EvalM a = EvalM {unEvalM :: ReaderT EvalInfo (StateT ForkCounter IO) a}
  deriving (Semigroup, Monoid) via (Ap EvalM a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadFix, MonadFail)

runEvalM :: EvalInfo -> EvalM a -> IO a
runEvalM info (EvalM m) = fst <$> runStateT (runReaderT m info) (ForkCounter 0)

data InterpretError = InterpretError !CallStack !SrcRange String

instance Show InterpretError where
  -- Ideally we would derive 'Show' (display the representation) and keep
  -- 'displayException' as the user-level textual output.
  --
  -- But the test suite, which is were most of these should crop up if at all,
  -- does not use 'displayException' but delegates to 'show'.
  show = displayException

instance Exception InterpretError where
  displayException (InterpretError cs p e) =
    concat
      [ if p == NullRange then "" else shows p ":",
        "interpret error: ",
        e,
        "\n\n",
        prettyCallStack cs
      ]

failInterpet :: (HasCallStack) => SrcRange -> String -> EvalM a
failInterpet !p = liftIO . throwIO . InterpretError callStack p

newtype ChannelId = ChannelId Word
  deriving stock (Show, Eq, Ord)

_ChannelId :: Lens' ChannelId Word
_ChannelId = coerced

data Channel = Channel
  { channelId :: !ChannelId,
    channelSide :: !Side,
    channelSend :: Value -> IO (),
    channelRecv :: IO Value
  }

instance Show Channel where
  show c = show (channelId c ^. _ChannelId) ++ "." ++ show (channelSide c)

-- | An indicator to differentiate the two channel endpoints.
data Side = A | B
  deriving stock (Show)

data Value
  = -- | The 'String' is a description of the closure value used in the 'Show'
    -- instance. The argument is annotated with its origin.
    Closure String (Located Value -> EvalM Value)
  | -- | A fully applied constructor. This includes pairs. See the 'Pair'
    -- pattern synonym for more information about their representation.
    Con !(ProgVar TcStage) [Value]
  | -- | Endpoint to a channel. The 'Side' is an indicator for the user.
    Endpoint !Channel
  | -- | Labels can't be constructed by the user. The are user to handle
    -- select/case operations on channels.
    Label !(ProgVar TcStage)
  | Number !Integer
  | String !String
  | Char !Char
  | Unit

-- | Pairs are represented through the 'Con' constructor with a name of 'PairConId'.
pattern Pair :: Value -> Value -> Value
pattern Pair a b <-
  Con ((conPair ==) -> True) [a, b]
  where
    Pair a b = Con conPair [a, b]

instance Show Value where
  showsPrec p =
    parenWords . \case
      Closure s _ ->
        [ showString "Closure",
          showChar '{' . showString s . showChar '}'
        ]
      Pair a b ->
        [ showString "Pair",
          showsPrec 11 a,
          showsPrec 11 b
        ]
      Con c vs ->
        [ showString "Con",
          showString (pprName c),
          showsPrec 11 vs
        ]
      Endpoint c ->
        [ showString "Endpoint",
          showsPrec 11 (channelId c),
          showsPrec 11 (channelSide c)
        ]
      Label lbl ->
        [ showString "Label",
          showString (pprName lbl)
        ]
      Number n -> unary "Number" n
      String s -> unary "String" s
      Char c -> unary "Char" c
      Unit -> [showString "Unit"]
    where
      unary :: (Show a) => String -> a -> [ShowS]
      unary label a = [showString label, showsPrec 11 a]
      parenWords :: [ShowS] -> ShowS
      parenWords [x] = x
      parenWords xs =
        List.intersperse (showChar ' ') xs
          & foldr (.) id
          & showParen (p > 10)

data Type a where
  TClosure :: Type (Located Value -> EvalM Value)
  TCon :: Type (ProgVar TcStage, [Value])
  TChannel :: Type Channel
  TLabel :: Type (ProgVar TcStage)
  TNumber :: Type Integer
  TString :: Type String
  TChar :: Type Char

{- ORMOLU_DISABLE -}
makeLenses ''EvalSt
stNextChannelL :: Lens' EvalSt ChannelId
stForkedL :: Lens' EvalSt ThreadList
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
makeLenses ['evalEnv] ''EvalInfo
evalEnvL :: Lens' EvalInfo Env
{- ORMOLU_ENABLE -}

formatThreadLog :: Bool -> ThreadName -> String -> P.Doc P.AnsiStyle
formatThreadLog colorize tname msg =
  setColorIfEnabled $ P.brackets (P.viaShow tname) P.<+> P.pretty msg
  where
    setColorIfEnabled =
      if colorize then P.annotate $! P.palette color else id
    color =
      fromIntegral $ (hash tname + 3) `rem` (228 - 21) + 21

-- | Outputs the given message. If debug colorization is enabled it will be
-- colorized using the current 'ThreadName'.
outputM :: String -> EvalM ()
outputM msg = do
  env <- EvalM ask
  outputDoc (evalOutputHandle (evalSettings env)) \wd ->
    P.layoutPretty (P.LayoutOptions wd) $
      formatThreadLog
        (evalDebugMessages (evalSettings env))
        (evalThreadName env)
        msg

-- | Outputs the given message if debug messages are enabled. The message is
-- colorized (if colorization is enabled) using the current 'ThreadName'.
debugLogM :: String -> EvalM ()
debugLogM msg = etaEvalM do
  env <- EvalM ask
  debugLog (Just (evalThreadName env)) (evalSettings env) msg

-- | Outputs the given message if 'evalDebugMessages' is not 'Nothing'.
--
-- If the wrapped value is 'Colorized' and a 'ThreadName' is given the message
-- is colorized based on a color unique to this thread. If either no
-- 'ThreadName' is given or the wrapped value is 'Plain' the message will be
-- printed without colors.
debugLog :: (MonadIO m) => Maybe ThreadName -> Settings -> String -> m ()
debugLog mname sett msg = when (evalDebugMessages sett) do
  outputDoc (evalOutputHandle sett) \wd ->
    P.layoutPretty (P.LayoutOptions wd) $
      maybe P.pretty (formatThreadLog True) mname msg

runEval :: Env -> EvalM a -> IO a
runEval = runEvalWith defaultSettings

runEvalWith :: Settings -> Env -> EvalM a -> IO a
runEvalWith settings env (EvalM m) = do
  let st0 =
        EvalSt
          { stNextChannel = ChannelId 0,
            stForked = []
          }
  let allThreads f ref = do
        -- While waiting for threads new threads might spawn.
        ts <- atomicModifyIORef' ref \st ->
          ( st {stForked = []},
            stForked st
          )
        case ts of
          [] -> pure ()
          _ -> traverse_ f ts *> allThreads f ref
  let info ref =
        EvalInfo
          { evalEnv = env,
            evalState = ref,
            evalSettings = settings,
            evalThreadName = ThreadName [] (showString "0")
          }
  let main ref = runEvalM (info ref) . EvalM $ do
        m
          <* liftIO (allThreads wait ref)
          <* debugLog Nothing settings "Evaluation Completed"
  let failed ref =
        debugLog Nothing settings "Evaluation Failed"
          *> allThreads cancel ref
  debugLog Nothing settings "Beginning Evaluation"
  bracketOnError (newIORef st0) failed main

etaEvalM :: EvalM a -> EvalM a
etaEvalM (EvalM m) = EvalM (etaReaderT m)
{-# INLINE etaEvalM #-}

askEnv :: EvalM Env
askEnv = EvalM (asks evalEnv)
{-# INLINE askEnv #-}

localEnv :: (Env -> Env) -> EvalM a -> EvalM a
localEnv f = EvalM . local (evalEnvL %~ f) . unEvalM
{-# INLINE localEnv #-}

askState :: EvalM (IORef EvalSt)
askState = EvalM (asks evalState)
{-# INLINE askState #-}

modifyState :: (EvalSt -> EvalSt) -> EvalM ()
modifyState f = do
  ref <- askState
  liftIO $ atomicModifyIORef' ref \st -> (f st, ())
{-# INLINE modifyState #-}

-- | Constructs the global 'Env' from a type checked 'Module'.
--
-- This always includes the bindings from 'builtinsModule'.
programEnvironment :: TcModule -> Env
programEnvironment p = builtinsEnv <> programEnv
  where
    -- To construct the program environment we are only interested in the
    -- module values.
    programValues =
      moduleValues builtinsModule <> moduleValues p
    programEnv =
      LMap.mapMaybeWithKey
        (\k -> either (conValue k) (globValue k))
        programValues

    conValue :: ProgVar TcStage -> ConstructorDecl Tc -> Maybe (Either TcExp Value)
    conValue !name = \case
      DataCon _ _ _ _ args ->
        -- Data constructors correspond to closures evaluating to a 'Con' value.
        --
        -- We could easily build a 'TcExp' value for the constructors but their
        -- 'Value' representation is simple enough (and does not require the
        -- full environment) to construct the 'Value' directly.
        --
        -- TODO: 'buildDataConType' in AlgST.Typing does basically the same. A
        -- unification of the logic in one place might be reasonable.
        let go :: tcType -> (Int -> DL.DList Value -> Value) -> Int -> DL.DList Value -> Value
            go _ f remaining vs =
              Closure
                (show remaining ++ "*" ++ show (Con name (DL.toList vs)))
                (\(_ :@ v) -> pure $ f (remaining - 1) (vs `DL.snoc` v))
         in Just $ Right $ foldr go (\_ -> Con name . DL.toList) args (length args) DL.empty
      ProtocolCon {} ->
        -- Protocol constructors can't appear as values after type checking.
        Nothing

    globValue :: ProgVar TcStage -> ValueDecl Tc -> Maybe (Either TcExp Value)
    globValue _ !d =
      -- The bodies of 'ValueDecl's (after TC) already contain the parameter
      -- lambda abstractions.
      Just . Left $ valueBody d

builtinsEnv :: Env
builtinsEnv =
  Map.fromList
    [ intFun opAdd \x y -> Number (x + y),
      intFun opSub \x y -> Number (x - y),
      intFun opMul \x y -> Number (x * y),
      intFun opDiv \x y -> Number (x `div` y),
      intFun opMod \x y -> Number (x `rem` y),
      intFun opLEQ \x y ->
        if x <= y
          then Con conTrue []
          else Con conFalse [],
      closure valSendLin \(_ :@ val) (p :@ channel) -> do
        c <- unwrap p TChannel channel
        putChannel c val
        pure channel,
      closure valReceive \(p :@ channel) -> do
        c <- unwrap p TChannel channel
        v <- readChannel c
        pure $ Pair v channel,
      closure valWait \(_ :@ _) -> do
        pure Unit :: EvalM Value,
      closure valTerminate \(_ :@ _) -> do
        pure Unit :: EvalM Value,
      closure valTrace \(_ :@ msgVal) -> do
        outputM (show msgVal)
        pure msgVal,
      closure valTraceMsg \(p :@ msgVal) -> do
        msg <- unwrap p TString msgVal
        outputM msg
        pure Unit,
      closure valUsleep \(p :@ durVal) -> do
        dur <- unwrap p TNumber durVal
        let effectiveDur =
              if dur > toInteger (maxBound :: Int)
                then maxBound
                else fromInteger dur
        liftIO $ threadDelay effectiveDur
        pure Unit :: EvalM Value
    ]
  where
    closure name body =
      (name, Right (buildClosure (pprName name) body))
    intFun name f = closure name \(p1 :@ a) (p2 :@ b) -> do
      a' <- unwrap p1 TNumber a
      b' <- unwrap p2 TNumber b
      pure $ f a' b'

evalLiteral :: E.Lit -> Value
evalLiteral = \case
  E.Unit -> Unit
  E.Int n -> Number n
  E.Char c -> Char c
  E.String s -> String s

-- | Evaluates the named top-level expression.
evalName :: (HasCallStack) => Name Resolved Values -> EvalM Value
evalName = lookupEnv NullRange

-- | Evaluates the given expression.
eval :: TcExp -> EvalM Value
eval =
  etaEvalM . \case
    E.Lit _ l -> do
      pure $ evalLiteral l
    E.Var p v -> do
      lookupEnv p v
    E.Con p c -> do
      lookupEnv p c

    --
    E.Abs _ bind -> do
      env <- askEnv
      pure $ bindClosure env bind

    --
    E.App _ e1 e2 -> do
      f <- evalAs TClosure e1
      x <- eval e2
      f (e2 @- x)

    --
    E.Pair _ e1 e2 -> do
      v1 <- eval e1
      v2 <- eval e2
      pure $ Pair v1 v2

    --
    E.Cond x _ _ _ -> do
      absurd x
    E.Case x _ _ -> do
      absurd x

    --
    E.TypeAbs _ (K.Bind _ _ _ e) -> do
      eval e
    E.TypeApp _ e _ -> do
      eval e

    --
    E.UnLet x _ _ _ _ -> do
      absurd x
    E.PatLet x _ _ _ _ -> do
      absurd x

    --
    E.Rec _ v _ rl -> do
      -- Like a lambda abstraction but `v` is bound in the body.
      env <- askEnv
      let env' = Map.insert v (Right val) env
          val = bindClosure env' $ recBody rl
      pure val

    -- Creates a new channel and returns a pair of the two endpoints.
    E.New _ _ -> do
      (c1, c2) <- newChannelPair
      pure $ Pair (Endpoint c1) (Endpoint c2)

    -- Constructs a function which sends the selected constructor as a label.
    -- The type abstractions are skipped as they correspond to no-ops anyways.
    E.Select _ (_ :@ con) -> do
      pure $ Closure ("select " ++ pprName con) \(appPos :@ c) -> do
        chan <- unwrap appPos TChannel c
        putChannel chan $ Label con
        pure c

    -- Forks the evaluation of `e` and sends the result over a new channel. In
    -- the current thread it evaluates to that new channel.
    E.Fork _ e -> do
      (c1, c2) <- newChannelPair
      forkEval e (putChannel c2)
      pure $ Endpoint c1

    -- Forks the evaluation of `e` and returns `Unit` in the current thread.
    E.Fork_ _ e -> do
      forkEval e \_ -> pure ()
      pure Unit

    --
    E.Exp (ValueCase p e cases) -> do
      val <- eval e
      if
        | Con con vs <- val,
          Just b <- Map.lookup con (E.casesPatterns cases) ->
            -- Bind the payload values.
            evalBranch b vs
        | Just b <- E.casesWildcard cases ->
            -- We have to allow any value to appear as the scrutinee in a case
            -- expression since `let` is desugared this way.
            --
            -- Bind the scrutinee itself.
            evalBranch b [val]
        | otherwise ->
            -- Something went wrong somewhere.
            failInterpet p $ "unmatchable value " ++ show val

    --
    E.Exp (RecvCase p e cases) -> do
      chanVal <- eval e
      channel <- unwrap (getRange e) TChannel chanVal
      l <- unwrap NullRange TLabel =<< readChannel channel
      b <-
        E.casesPatterns cases
          & Map.lookup l
          & maybe (unmatchableConstructor p l) pure
      localBinds [(v, chanVal) | _ :@ v <- toList (E.branchBinds b)] do
        eval $ E.branchExp b

evalBranch :: (Foldable f) => E.CaseBranch f Tc -> [Value] -> EvalM Value
evalBranch b vs =
  localBinds (zip (unL <$> toList (E.branchBinds b)) vs) do
    eval $ E.branchExp b

newChannelPair :: EvalM (Channel, Channel)
newChannelPair = do
  env <- EvalM ask
  cid <- liftIO $ atomicModifyIORef (evalState env) \st ->
    ( st & stNextChannelL . _ChannelId +~ 1,
      st ^. stNextChannelL
    )
  liftIO $ makeChannelPair (evalBufferSize (evalSettings env)) cid
  where
    makeChannelPair = \case
      0 ->
        -- No buffering at all. Create a synchronous pair.
        newSyncChannelPair
      1 ->
        -- Use simple MVars to simulate a buffer of one.
        -- (This is an optimization over the STM bounded queue.)
        newChannelPair'
          newEmptyMVar
          putMVar
          takeMVar
      n ->
        -- Use a bounded queue for any bigger buffer.
        newChannelPair'
          (newTBQueueIO n)
          (fmap atomically . writeTBQueue)
          (atomically . readTBQueue)

-- | Creates a pair of synchronous channels. It uses one 'MVar' to transfer the
-- value either way, and a second 'MVar' to signal that the value was received.
newSyncChannelPair :: ChannelId -> IO (Channel, Channel)
newSyncChannelPair cid = do
  valueVar <- newEmptyMVar
  syncVar <- newEmptyMVar
  let send v =
        putMVar valueVar v
          <* takeMVar syncVar
  let recv =
        takeMVar valueVar
          <* putMVar syncVar ()
  pure (Channel cid A send recv, Channel cid B send recv)

-- | Generalized function to create a pair of channels with any buffer with
-- size of at least /one./
--
-- This function creates two @queue@s using the provided action and constructs
-- the 'Channel's such that one writes into one queue and reads from the other
-- and the second 'Channel' does vice versa.
newChannelPair' ::
  -- | Action to create one queue. Executed twice.
  IO queue ->
  -- | Action to write a value into the queue.
  (queue -> Value -> IO ()) ->
  -- | Action to read the first value from the queue.
  (queue -> IO Value) ->
  ChannelId ->
  IO (Channel, Channel)
newChannelPair' mkQueue writeQueue readQueue cid = do
  q1 <- mkQueue
  q2 <- mkQueue
  let channel :: (forall a. a -> a -> a) -> Channel
      channel sel =
        Channel
          { channelId = cid,
            channelSide = sel A B,
            channelSend = writeQueue (sel q1 q2),
            channelRecv = readQueue (sel q2 q1)
          }
  pure (channel const, channel (const id))

putChannel :: Channel -> Value -> EvalM ()
putChannel c v = do
  debugLogM $ "╔ send@" ++ show c ++ ": " ++ show v
  liftIO $ channelSend c v
  debugLogM $ "╚ send@" ++ show c

readChannel :: Channel -> EvalM Value
readChannel c = do
  debugLogM $ "╭ recv@" ++ show c
  v <- liftIO $ channelRecv c
  debugLogM $ "╰ recv@" ++ show c ++ ": " ++ show v
  pure v

forkEval :: TcExp -> (Value -> EvalM ()) -> EvalM ()
forkEval e f = do
  childId <- EvalM $ get <* modify' (+ 1)
  parentEnv <- EvalM ask
  let settings = evalSettings parentEnv
  let parentName = evalThreadName parentEnv
  let childName = deriveChildName parentName childId
  let childEnv = parentEnv {evalThreadName = childName}
  -- Fork evaluation.
  thread <- liftIO . mask_ $ asyncWithUnmask \restore -> do
    debugLog (Just childName) settings ("┏ starting from " ++ show parentName)
    restore (runEvalM childEnv (f =<< eval e)) `catch` \(e :: SomeException) -> do
      debugLog (Just childName) settings $ "┗ failed: " ++ displayException e
      throwIO e
    debugLog (Just childName) settings "┗ completed"
  -- Record the forked thread.
  modifyState \st -> do
    st & stForkedL %~ (thread :)

recBody :: E.RecLam Tc -> E.Bind Tc
recBody (E.RecTermAbs _ bind) = bind
recBody (E.RecTypeAbs _ (K.Bind _ _ _ rl)) = recBody rl

bindClosure :: Env -> TcBind -> Value
bindClosure env bind@(E.Bind _ _ v _ body) =
  buildClosure (show bind) \(_ :@ !a) -> do
    let !env' = Map.insert v (Right a) env
    localEnv (const env') $ eval body

-- Establish a set of bindings locally.
localBinds :: [(ProgVar TcStage, Value)] -> EvalM a -> EvalM a
localBinds binds = localEnv \e -> Right `fmap` Map.fromList binds <> e

-- | Looks for the given variable in the current environment. If it resovles to
-- a top-level expression it will be evaluated before returning.
lookupEnv :: (HasCallStack) => SrcRange -> ProgVar TcStage -> EvalM Value
lookupEnv p v =
  askEnv
    >>= maybe (failInterpet p err) pure . Map.lookup v
    >>= either eval pure
  where
    err = pprName v ++ " (" ++ pprResolved v ++ ") is not bound."

-- | Evaluates the given expression and extracts the expected type.
evalAs :: Type a -> TcExp -> EvalM a
evalAs ty e = eval e >>= unwrap (getRange e) ty

-- | Tries to extract the payload of the given type from a value. If the value
-- has a different type an 'InterpretError' will be thrown.
unwrap :: SrcRange -> Type a -> Value -> EvalM a
unwrap _ TNumber (Number n) = pure n
unwrap _ TString (String s) = pure s
unwrap _ TChar (Char c) = pure c
unwrap _ TClosure (Closure _ f) = pure f
unwrap _ TCon (Con c vs) = pure (c, vs)
unwrap _ TLabel (Label l) = pure l
unwrap _ TChannel (Endpoint c) = pure c
unwrap p ty v =
  failInterpet p . unwords $
    [ "expected",
      tyname,
      "but the value is\n\t",
      show v
    ]
  where
    tyname = case ty of
      TCon -> "a data value"
      TChannel -> "a channel"
      TLabel -> "a label"
      TNumber -> "a number"
      TString -> "a string"
      TChar -> "a char"
      TClosure -> "a closure"

unmatchableConstructor :: SrcRange -> ProgVar TcStage -> EvalM a
unmatchableConstructor p c = failInterpet p $ "unmatchable constructor " ++ pprName c
{-# NOINLINE unmatchableConstructor #-}

class BuildClosure a where
  buildClosureS :: ShowS -> a -> Value

instance (a ~ Located Value, r ~ Value) => BuildClosure (a -> EvalM r) where
  buildClosureS d = Closure (d "")

instance
  (a ~ Located Value, BuildClosure (b -> r)) =>
  BuildClosure (a -> b -> r)
  where
  buildClosureS d body = Closure (d "") \arg ->
    pure $ buildClosureS (d . showChar ' ' . showsPrec 11 (unL arg)) (body arg)

buildClosure :: (BuildClosure a) => String -> a -> Value
buildClosure = buildClosureS . showString
