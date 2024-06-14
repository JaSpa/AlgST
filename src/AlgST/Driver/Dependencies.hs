{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}

module AlgST.Driver.Dependencies
  ( DepVertex,
    Cycles (..),
    DepsTree,
    DepsGraph,
    emptyDepsGraph,
    depsGraphSize,
    depsVertices,
    Dependency (DependsOn),
    insertModule,
    insertDependency,
    lookupVertex,
    depsMember,
    removeCycles,
    traverseTreePar,
    traverseVerticesPar,
    ImportLocation (..),
    importLocPath,

    -- * Visualizing the Dependency Graph
    exportTextual,
  )
where

import AlgST.Syntax.Name
import AlgST.Util.SourceLocation
import Algebra.Graph.AdjacencyMap qualified as G
import Algebra.Graph.AdjacencyMap.Algorithm qualified as G
import Control.Category ((>>>))
import Control.Monad.IO.Unlift
import Control.Scheduler
import Data.Coerce
import Data.Foldable
import Data.Function
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable
import Data.IORef
import Data.Kind
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.Map qualified as Map
import Data.Ord
import Data.Semigroup
import Data.Set qualified as Set
import Data.Traversable
import GHC.Generics (Generic)

newtype ImportLocation = ImportLocation (Located FilePath) -- TODO: check if the file path is actually still needed
  deriving stock (Generic)
  deriving (HasRange) via Generically ImportLocation

instance Show ImportLocation where
  show (ImportLocation (p :@ fp))
    | null fp = "???:" ++ show p
    | otherwise = fp ++ ':' : show p

-- | Two 'ImportLocation' values are considered equal if their contained
-- 'SrcRange' values are equal.
--
-- The stored 'FilePath' is not considered. this is mostly a slight
-- optimization since all edge labels from a module should originate from the
-- same source file.
instance Eq ImportLocation where
  a == b = compare a b == EQ

-- | See 'Eq' instance.
instance Ord ImportLocation where
  compare = comparing getRange

instance Semigroup ImportLocation where
  a <> b = mconcat [a, b]
  sconcat = toList >>> mconcat
  stimes = stimesIdempotentMonoid

instance Monoid ImportLocation where
  mempty = ImportLocation $ NullRange @- ""
  mconcat = filter (/= mempty) >>> nonEmpty >>> foldMap minimum

importLocPath :: ImportLocation -> FilePath
importLocPath (ImportLocation iloc) = unL iloc

data Cycles
  = Acyclic
  | MaybeCyclic

type DepVertex = ModuleName

type DepsTree = DepsGraph Acyclic

type DepsGraph :: Cycles -> Type -> Type
data DepsGraph cycles a = DepsGraph
  { -- | A map from @(x,y)@ to the location where @x@ imported @y@.
    dgEdges :: !(HashMap Dependency ImportLocation),
    -- | Each vertex can be associated with additional data.
    dgVertices :: !(HashMap DepVertex a),
    -- | Vertices are conntected to the vertices they depend on.
    --
    -- E.g. @X@ depends on @Y@ and @Z@, and @Y@ depends on @Z@ gives
    --
    -- >  Z <-- Y
    -- >  ^     ^
    -- >  |     |
    -- >  +---- X
    --
    -- This is the transposition of 'dgVerticesToUsedBy'.
    dgVerticesToDeps :: !(G.AdjacencyMap DepVertex),
    -- | Vertices are conncted to the vertices they are used by.
    --
    -- E.g. @X@ depends on @Y@ and @Z@, and @Y@ depends on @Z@ gives
    --
    -- >  Z --> Y
    -- >  |     |
    -- >  |     v
    -- >  +---> X
    --
    -- This is the transposition of 'dgVerticesToDeps'.
    dgVerticesToUsedBy :: !(G.AdjacencyMap DepVertex)
  }
  deriving stock (Functor)

-- | The empty 'DepsGraph'.
emptyDepsGraph :: DepsGraph cycles a
emptyDepsGraph = DepsGraph HM.empty HM.empty G.empty G.empty

-- | The number of vertices, i.e. distinct modules, in the graph.
depsGraphSize :: DepsGraph cycles a -> Int
depsGraphSize = dgVerticesToDeps >>> G.vertexCount

-- | Check if the given node is recorded in the dependency graph.
depsMember :: DepVertex -> DepsGraph acyclic a -> Bool
depsMember v dg = G.hasVertex v (dgVerticesToDeps dg)

depsVertices :: DepsGraph acyclic a -> HashMap DepVertex a
depsVertices = dgVertices

lookupVertex :: DepVertex -> DepsGraph acyclic a -> Maybe a
lookupVertex v = HM.lookup v . depsVertices

-- | Data type to help with understanding the direction of the depedency.
-- Consider using the constructor in its infix form:
--
-- > x `DependsOn` y
newtype Dependency = Dependency (DepVertex, DepVertex)
  deriving newtype (Eq, Ord, Hashable)

pattern DependsOn :: DepVertex -> DepVertex -> Dependency
pattern x `DependsOn` y = Dependency (x, y)

{-# COMPLETE DependsOn #-}

infix 6 `DependsOn`

instance Show Dependency where
  showsPrec p (x `DependsOn` y) =
    showParen (p > prec) $
      showsPrec (prec + 1) x . showString " `DependsOn` " . showsPrec (prec + 1) y
    where
      prec = 6

-- | Record a vertex in the dependency graph.
insertModule :: DepVertex -> a -> DepsGraph cycles a -> DepsGraph cycles a
insertModule v a dg =
  DepsGraph
    { -- No new edges.
      dgEdges = dgEdges dg,
      -- But make sure the vertex exists.
      dgVertices = HM.insert v a (dgVertices dg),
      dgVerticesToDeps = G.vertex v <> dgVerticesToDeps dg,
      dgVerticesToUsedBy = G.vertex v <> dgVerticesToUsedBy dg
    }

-- | Records a dependency in the graph.
insertDependency ::
  ImportLocation ->
  Dependency ->
  DepsGraph cycles a ->
  DepsGraph MaybeCyclic a
insertDependency loc (x `DependsOn` y) dg =
  DepsGraph
    { dgVertices = dgVertices dg,
      dgEdges = HM.insertWith (<>) (x `DependsOn` y) loc (dgEdges dg),
      dgVerticesToDeps = G.edge x y <> dgVerticesToDeps dg,
      dgVerticesToUsedBy = G.edge y x <> dgVerticesToUsedBy dg
    }

-- | Removes edges from the graph until it is acyclic.
--
-- The order of returned cycles is unspecified. The first edge of each cycle
-- corresponds to edge missing from the resulting graph.
removeCycles ::
  DepsGraph cycles a ->
  (DepsGraph Acyclic a, [G.Cycle (DepVertex, ImportLocation)])
removeCycles dg0@DepsGraph {dgEdges = labels} = go [] dg0
  where
    go cs dg = case G.topSort (dgVerticesToDeps dg) of
      Left c -> go (labelCycle c : cs) (breakCycle c dg)
      Right _ -> (coerce dg, cs)
    breakCycle c dg = do
      let (x, y) = case c of
            v :| [] -> (v, v)
            v :| w : _ -> (v, w)
      DepsGraph
        { dgVertices = dgVertices dg,
          dgEdges = HM.delete (x `DependsOn` y) (dgEdges dg),
          dgVerticesToDeps = G.removeEdge x y (dgVerticesToDeps dg),
          dgVerticesToUsedBy = G.removeEdge y x (dgVerticesToUsedBy dg)
        }
    labelCycle (v0 :| vs) = do
      let lookupLabel x y =
            fold $ HM.lookup (x `DependsOn` y) labels
      let f x (dep, annots) =
            (x, (x, lookupLabel x dep) : annots)
      let (v1, annots) = foldr f (v0, []) vs
      (v0, lookupLabel v0 v1) :| annots

data TraverseState a = TraverseState !Int [a]

-- | Execute an action for each node in the depdency graph. The execution is
-- paralellized as much as possible.
--
-- Each action gets the results from its dependencies as inputs. The result
-- corresponds to the list of all action outputs. The ordering of the list is
-- unpsecified.
--
-- Note that the computation strategy 'Seq' will degrade to @'ParN' 1@ based on
-- implementation requirements.
traverseTreePar ::
  forall a b m.
  (MonadUnliftIO m) =>
  Comp ->
  DepsTree a ->
  ([(DepVertex, b)] -> DepVertex -> Maybe a -> m b) ->
  m (DepsTree b)
traverseTreePar strat dg op =
  buildGraph <$> withScheduler strat' \s -> do
    askRunInIO >>= \runIO -> liftIO mdo
      -- For each vertex we create an action which waits for N inputs. When the
      -- N-th input arrives it runs `op`.
      --
      -- When `op` completes the action is tasked with sending the result to each
      -- depending vertex-action.
      let runOp :: [(DepVertex, b)] -> DepVertex -> IO (DepVertex, b)
          runOp bs v = do
            b <- runIO $ op bs v (HM.lookup v (dgVertices dg))
            for_ (G.postSet v (dgVerticesToUsedBy dg)) \v' ->
              for_ (Map.lookup v' actions) \f ->
                f v b
            pure (v, b)
      let superflousInput =
            fail "received input for already scheduled vertex"
      let vertexAction :: DepVertex -> Set.Set dep -> IO (DepVertex -> b -> IO ())
          vertexAction a deps
            | Set.null deps = do
                scheduleWork s $ runOp [] a
                pure \_ _ -> superflousInput
            | otherwise = do
                r <- newIORef $ Just $ TraverseState (Set.size deps) []
                pure \v b -> do
                  bs <- atomicModifyIORef' r \case
                    Nothing ->
                      (Nothing, Nothing)
                    Just (TraverseState 1 bs) ->
                      (Nothing, Just ((v, b) : bs))
                    Just (TraverseState n bs) ->
                      (Just $! TraverseState (n - 1) ((v, b) : bs), Just [])
                  case bs of
                    Just [] -> pure ()
                    Just bs -> scheduleWork s $ runOp bs a
                    Nothing -> superflousInput
      actions <-
        dgVerticesToDeps dg
          & G.adjacencyMap
          & Map.traverseWithKey vertexAction
      pure ()
  where
    buildGraph results =
      dg {dgVertices = HM.fromList results}
    strat' = case strat of
      Seq -> ParN 1
      _ -> strat

traverseVerticesPar ::
  (MonadUnliftIO m) =>
  Comp ->
  (DepVertex -> a -> m b) ->
  DepsGraph cycles a ->
  m (DepsGraph cycles b)
traverseVerticesPar comp f dg = withRunInIO \run -> do
  -- `traverseConcurrently` from the 'scheduler' package adapted for an indexed
  -- traversal.
  bs <- withScheduler comp \s ->
    HM.traverseWithKey (\k -> scheduleWork s . run . f k) (dgVertices dg)
  let nextB (b : bs) _ = (bs, b)
      nextB _ _ = error "impossible: 2nd traverse has different shape"
  let (_, bVertices) = mapAccumL nextB bs (dgVertices dg)
  pure dg {dgVertices = bVertices}

exportTextual :: DepsGraph cycles a -> String
exportTextual dg =
  dgVerticesToDeps dg
    & G.adjacencyMap
    & Map.foldMapWithKey go
    & flip appEndo ""
  where
    -- Count the maximum vertex width to create aligned ouput.
    maxlen =
      dgVerticesToDeps dg
        & G.vertexList
        & fmap (length . unModuleName)
        -- `maximum` diverges if there are no vertices but `maxlen` will only
        -- be forced iff there is at least one vertex.
        & maximum
        -- Increase by one for space after.
        & (+ 1)
    padded (ModuleName v) =
      showString (padString maxlen v)
    go v deps
      | Set.null deps =
          Endo $ showString (unModuleName v) . showChar '\n'
      | otherwise = fold do
          let edge d =
                padded v . showString "--> " . padded d
          let renderIloc d =
                showString $ maybe "???" show $ HM.lookup (v `DependsOn` d) (dgEdges dg)
          let render d =
                edge d . showString " [" . renderIloc d . showString "]\n"
          [Endo $ render d | d <- Set.toList deps]

padString :: Int -> String -> String
padString n s = take n $ s ++ repeat ' '
