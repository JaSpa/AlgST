{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QualifiedDo #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}

module TypingSpec (spec) where

import AlgST.Builtins
import AlgST.Parse.Parser
import AlgST.Parse.Phase
import AlgST.Rename
import AlgST.Syntax.Kind qualified as K
import AlgST.Syntax.Module
import AlgST.Syntax.Name
import AlgST.Syntax.Traversal
import AlgST.Syntax.Tree
import AlgST.Typing
import AlgST.Typing.Equality qualified as Eq
import AlgST.Util.Error
import Control.DeepSeq
import Control.Exception
import Control.Monad
import Data.Bifunctor
import Data.DList.DNonEmpty qualified as DNE
import Data.Foldable
import Data.Function
import Data.List.NonEmpty (NonEmpty)
import Language.Haskell.TH.CodeDo qualified as Code
import System.FilePath
import Test.Golden
import Test.Hspec

spec :: Spec
spec = do
  describe "normal form" do
    it "normalises parameters" do
      "forall (p:P). D3 -(+(-(+p))) (!-p.end) (forall (a:TU) (b:TU). a -> b)"
        `nfShouldBe` "forall (p:P). D3 p (?p.end) (forall (a:TU). a -> forall (b:TU). b)"

    context "forall isomorphism" do
      it "pushes foralls down" do
        "forall (a:TU). D0 -> forall (b:TU). a -> b"
          `nfShouldBe` "D0 -> forall (a:TU). a -> forall (b:TU). b"
      it "splits multiple foralls" do
        "forall (a:TU) (b:TU). a -> b"
          `nfShouldBe` "forall (a:TU). a -> forall (b:TU). b"
      it "doesn't reorder foralls" do
        "forall (a:TU) (b:TU). b -> a"
          `nfShouldBe` "forall (a:TU) (b:TU). b -> a"

  describe "whole programs" do
    describe "invalid" do
      goldenTests
        (dir "invalid/prog")
        (swap . bimap plainErrors drawLabeledTree . parseAndCheckProgram)

  describe "kind checking" do
    specify "builtin types" do
      "()" `kindShouldBe` K.MU
      "Int" `kindShouldBe` K.MU
      "Char" `kindShouldBe` K.MU
      "String" `kindShouldBe` K.MU
      -- Enumeration types could be very sensibly be MU by default.
      "Bool" `kindShouldBe` K.TU

    specify "declared new types" do
      "D0" `kindShouldBe` K.TU
      "D0'" `kindShouldBe` K.TU
      "D0_TL" `kindShouldBe` K.TL
      "P0" `kindShouldBe` K.P
      "P0'" `kindShouldBe` K.P

    specify "unbound variables don't crash" do
      void $ evaluate $ force $ show (performKiSynth "x")

    context "syntax elements" do
      specify "forall" do
        "forall (a:MU). a" `kindShouldBe` K.TU
        "forall (a:SL). a" `kindShouldBe` K.TL

      specify "arrow" do
        "D0_TL -> ()" `kindShouldBe` K.TU
        "() -o D0" `kindShouldBe` K.TL

      specify "pairs" do
        -- Is always *at least* TU.
        "( (), () )" `kindShouldBe` K.TU
        "( (), D0 )" `kindShouldBe` K.TU
        "( D0, () )" `kindShouldBe` K.TU
        "( D0, D0 )" `kindShouldBe` K.TU
        -- If one component is linear the whole pair is linear.
        "( (), D0_TL )" `kindShouldBe` K.TL
        "( D0_TL, () )" `kindShouldBe` K.TL
        "( D0_TL, D0_TL )" `kindShouldBe` K.TL

      specify "session types" do
        let checks =
              [ ("end", kindShouldBe, K.SU),
                ("!() . end", kindShouldBe, K.SL),
                ("?D0 . end", kindShouldBe, K.SL)
              ]

        for_ checks \(s, f, k) -> do
          f s k

        for_ checks \(s, f, k) -> do
          f ("dual (" ++ s ++ ")") k

    context "type application" do
      specify "same kind application" do
        "Id_MU ()" `kindShouldBe` K.MU

      specify "subkind application" do
        "Id_TL ()" `kindShouldBe` K.TL

      specify "multi-param application" do
        -- D3/P3 each take one P, one S, one TL.
        "forall (p:P).  D3 p (!p.end) ()" `kindShouldBe` K.TU
        "forall (p:P). ?P3 p (!p.end) ().end" `kindShouldBe` K.TL

      specify "nested application" do
        "Id_TL (Id_TL (Id_MU ()))" `kindShouldBe` K.TL

    describe "invalid" do
      goldenTests
        (dir "invalid/kinds")
        (swap . fmap show . performKiSynth)

  describe "type checking" do
    describe "valid" do
      goldenTests
        (dir "valid/prog")
        (bimap plainErrors drawLabeledTree . parseAndCheckProgram)

    parallel $ describe "invalid" do
      goldenTests
        (dir "invalid/types")
        (swap . fmap show . performTySynth)

infix 1 `nfShouldBe`, `kindShouldBe`

shouldNotError :: (HasCallStack, Foldable f) => Either (f Diagnostic) a -> IO a
shouldNotError = \case
  Left errs -> expectationFailure (plainErrors errs) >> mzero
  Right a -> pure a

kindShouldBe :: HasCallStack => String -> K.Kind -> Expectation
kindShouldBe tyStr k =
  -- Use kisynth + manual check because kicheck allows for a mismatch which is
  -- covered up by the subkinding relationship.
  case runKiAction parseType (\_ ty -> kisynth ty) tyStr of
    Left e -> expectationFailure e
    Right (_, k') -> when (k /= k') do
      expectationFailure $ "[expected] " <> show k <> " /= " <> show k' <> " [kisynth]"

nfShouldBe :: HasCallStack => String -> String -> Expectation
nfShouldBe t1 t2 = do
  (t1NF, t2Tc) <- shouldNotError do
    t1' <- runParser parseType t1
    t2' <- runParser parseType t2

    let (_, getExtra) = renameModuleExtra (ModuleName "M") emptyModule
    RenameExtra f <- first DNE.toNonEmpty $ getExtra fullEnv
    first DNE.toNonEmpty $
      f $ const do
        t1Rn <- renameSyntax t1'
        t2Rn <- renameSyntax t2'
        checkResultAsRnM $ checkWithModule fullCtxt emptyModule \_ _ -> do
          (t1Tc, _) <- kisynth t1Rn
          (t2Tc, _) <- kisynth t2Rn
          t1NF <- normalize t1Tc
          pure (t1NF, t2Tc)

  when (Eq.Alpha t1NF /= Eq.Alpha t2Tc) do
    expectationFailure $
      unlines
        [ "normal forms do not match.",
          "\texpected: " ++ show t2Tc,
          "\tactual:   " ++ show t1NF
        ]

performKiSynth :: String -> Either String K.Kind
performKiSynth = runKiAction parseType (\_ -> fmap snd . kisynth)

performTySynth :: String -> Either String TcType
performTySynth = runKiAction parseExpr (\embed -> fmap snd . embed . tysynth)

-- | Parses the string with the given parser, renames it in the context of
-- 'declarations' and runs the given 'KindM' action.
runKiAction ::
  SynTraversable (s Parse) Parse (s Rn) Rn =>
  Parser (s Parse) ->
  ( forall env st.
    (HasKiEnv env, HasKiSt st) =>
    RunTyM env st ->
    s Rn ->
    TcM env st a
  ) ->
  String ->
  Either String a
runKiAction p m src = first plainErrors do
  parsed <- runParser p src
  let (_, getExtra) = renameModuleExtra (ModuleName "M") emptyModule
  RenameExtra f <- first DNE.toNonEmpty $ getExtra fullEnv
  first DNE.toNonEmpty $
    f $ const do
      renamed <- renameSyntax parsed
      checkResultAsRnM $ checkWithModule fullCtxt emptyModule \runTypeM _ -> do
        m runTypeM renamed

parseAndCheckProgram :: String -> Either (NonEmpty Diagnostic) (Module Tc)
parseAndCheckProgram src = do
  parsed <- runParser parseDecls src
  let (_, getExtra) = renameModuleExtra (ModuleName "M") parsed
  RenameExtra f <- first DNE.toNonEmpty $ getExtra fullEnv
  first DNE.toNonEmpty $ f $ checkResultAsRnM . checkModule fullCtxt

declEnv :: RenameEnv
declCtxt :: CheckContext
(declEnv, declCtxt) =
  $$( Code.do
        let src =
              [ "data D0         = D0",
                "data D0'   : TU = D0'",
                "data D0_TL : TL = D0_TL",
                --
                "protocol P0      = P0",
                "protocol P0' : P = P0'",
                --
                "type Id_MU : MU (a:MU) = a",
                "type Id_TU : TU (a:TU) = a",
                "type Id_TL : TL (a:TL) = a",
                --
                "data     D3 (a:P) (b:SL) (c:TL) = D3",
                "protocol P3 (a:P) (b:SL) (c:TL) = P3",
                --
                "type Session (x:P) = forall (s:SL). ?x.s -> s",
                --
                "data AB = A | B"
              ]

        parsed <-
          unlines src
            & runParserSimple parseDecls
            & either fail pure
        let name = ModuleName "Declarations"
        let (declMM, mkRn) = renameModuleExtra name parsed
        let checkRes = do
              RenameExtra f <- mkRn builtinsEnv
              f \renamed -> do
                checkResultAsRnM $ checkWithModule
                  builtinsModuleCtxt
                  renamed
                  \runTypeM _ -> runTypeM extractCheckContext
        ctxt <- either (fail . plainErrors) pure checkRes
        let checkEnv = importAllEnv ZeroPos name declMM emptyModuleName
        [||(checkEnv, ctxt)||]
    )

fullEnv :: RenameEnv
fullEnv = builtinsEnv <> declEnv

fullCtxt :: CheckContext
fullCtxt = builtinsModuleCtxt <> declCtxt

dir :: FilePath -> FilePath
dir sub = dropExtension __FILE__ </> sub
