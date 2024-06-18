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
import AlgST.Typing.Align
import AlgST.Util.SourceManager
import Control.Monad
import Data.Foldable
import Language.Haskell.TH.CodeDo qualified as Code
import System.FilePath
import Test
import Test.Golden

spec :: Spec
spec = do
  describe "normal form" do
    it "dual End! ~ End?" do
      "dual End!" `nfShouldBe` "End?"

    it "dual End? ~ End!" do
      "dual End?" `nfShouldBe` "End!"

    it "normalises parameters" do
      "forall (p:P). D3 -(+(-(+p))) (!-p.End!) (forall (a:TU) (b:TU). a -> b)"
        `nfShouldBe` "forall (p:P). D3 p (?p.End!) (forall (a:TU). a -> forall (b:TU). b)"

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
        (plainErrorsM <=< expectDiagnostics_ . parseAndCheckProgram)

  describe "kind checking" do
    specify "builtin types" do
      "()" `kindShouldBe` K.TU
      "Int" `kindShouldBe` K.TU
      "Char" `kindShouldBe` K.TU
      "String" `kindShouldBe` K.TU
      "Bool" `kindShouldBe` K.TU
      -- Since we have explicit wait/terminate our End types have to be linear!
      "End!" `kindShouldBe` K.SL
      "End?" `kindShouldBe` K.SL

    specify "declared new types" do
      "D0" `kindShouldBe` K.TU
      "D0'" `kindShouldBe` K.TU
      "D0_TL" `kindShouldBe` K.TL
      "P0" `kindShouldBe` K.P
      "P0'" `kindShouldBe` K.P

    specify "unbound variables don't crash" do
      void $ expectDiagnostics_ $ performKiSynth "x"

    context "syntax elements" do
      specify "forall" do
        "forall (a:TU). a" `kindShouldBe` K.TU
        "forall (a:TL). a" `kindShouldBe` K.TL
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
              [ ("!() . End!", kindShouldBe, K.SL),
                ("?D0 . End!", kindShouldBe, K.SL)
              ]

        for_ checks \(s, f, k) -> do
          f s k

        for_ checks \(s, f, k) -> do
          f ("dual (" ++ s ++ ")") k

    context "type application" do
      specify "subkind application" do
        "Id_TL ()" `kindShouldBe` K.TL

      specify "multi-param application" do
        -- D3/P3 each take one P, one S, one TL.
        "forall (p:P).  D3 p (!p.End!) ()" `kindShouldBe` K.TU
        "forall (p:P). ?P3 p (!p.End!) ().End!" `kindShouldBe` K.TL

      specify "nested application" do
        "Id_TL (Id_TL (Id_TU ()))" `kindShouldBe` K.TL

    describe "invalid" do
      goldenTests
        (dir "invalid/kinds")
        (expectDiagnostics . performKiSynth)

  describe "type checking" do
    describe "valid" do
      goldenTests
        (dir "valid/prog")
        (fmap drawLabeledTree . parseAndCheckProgram)

    parallel $ describe "invalid" do
      goldenTests
        (dir "invalid/types")
        (expectDiagnostics . performTySynth)

infix 1 `nfShouldBe`, `kindShouldBe`

kindShouldBe :: (HasCallStack) => String -> K.Kind -> Assertion ()
kindShouldBe tyStr k = do
  -- Use kisynth + manual check because kicheck allows for a mismatch which is
  -- covered up by the subkinding relationship.
  (_, k') <- runKiAction parseType (\_ ty -> kisynth ty) tyStr
  when (k /= k') do
    expectationFailure $ "[expected] " <> show k <> " /= " <> show k' <> " [kisynth]"

nfShouldBe :: (HasCallStack) => String -> String -> Assertion ()
nfShouldBe t1 t2 = do
  t1Ty <- shouldParse parseType t1
  t2Ty <- shouldParse parseType t2
  (t1NF, t2Tc) <- withRenameContext' fullEnv do
    t1Rn <- renameSyntax t1Ty
    t2Rn <- renameSyntax t2Ty
    withTcContext' fullCtxt \_ -> do
      (t1Tc, _) <- kisynth t1Rn
      (t2Tc, _) <- kisynth t2Rn
      t1NF <- normalize t1Tc
      pure (t1NF, t2Tc)

  when (Alpha t1NF /= Alpha t2Tc) do
    expectationFailure $
      unlines
        [ "normal forms do not match.",
          "\texpected: " ++ show t2Tc,
          "\tactual:   " ++ show t1NF
        ]

performKiSynth :: String -> Assertion K.Kind
performKiSynth = runKiAction parseType (\_ -> fmap snd . kisynth)

performTySynth :: String -> Assertion TcType
performTySynth = runKiAction parseExpr (\embed -> fmap snd . embed . tysynth)

-- | Parses the string with the given parser, renames it in the context of
-- 'declarations' and runs the given 'KindM' action.
runKiAction ::
  (SynTraversable Parse Rn (s Parse) (s Rn)) =>
  Parser (s Parse) ->
  ( forall env st.
    (HasKiEnv env, HasKiSt st) =>
    RunTyM env st ->
    s Rn ->
    TcM env st a
  ) ->
  String ->
  Assertion a
runKiAction p m src = do
  parsed <- shouldParse p src
  withRenameContext' fullEnv do
    renamed <- renameSyntax parsed
    withTcContext' fullCtxt \runTypeM -> do
      m runTypeM renamed

parseAndCheckProgram :: String -> Assertion (Module Tc)
parseAndCheckProgram src = do
  ParsedModule pimports parsed <- shouldParse parseModule src
  when (not (null pimports)) do
    expectationFailure "imports are not supported"
  let (_, getExtra) = renameModuleExtra (ModuleName "M") parsed
  RenameExtra f <- shouldNotError $ getExtra fullEnv
  shouldNotError $ f $ checkResultAsRnM . checkModule fullCtxt

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

        let buf = encodedBuffer "«input»" (unlines src)
        let failErrors :: (Foldable f, MonadFail m) => f Diagnostic -> m a
            failErrors = fail . plainErrors (singletonManager buf)
        parsed <- either failErrors pure do
          runParser parseDecls (bufferContents buf)
        let name = ModuleName "Declarations"
        let (declMM, mkRn) = renameModuleExtra name parsed
        let checkRes = do
              RenameExtra f <- mkRn builtinsEnv
              f \renamed -> do
                checkResultAsRnM $ checkWithModule
                  builtinsModuleCtxt
                  renamed
                  \runTypeM _ -> runTypeM extractCheckContext
        ctxt <- either failErrors pure checkRes
        let checkEnv = importAllEnv NullRange name declMM emptyModuleName
        [||(checkEnv, ctxt)||]
    )

fullEnv :: RenameEnv
fullEnv = builtinsEnv <> declEnv

fullCtxt :: CheckContext
fullCtxt = builtinsModuleCtxt <> declCtxt

dir :: FilePath -> FilePath
dir sub = dropExtension __FILE__ </> sub
