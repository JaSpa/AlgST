{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module TypingRelationSpec where

import AlgST.Parse.Parser
import AlgST.Parse.Phase
import AlgST.Rename
import AlgST.Syntax.Kind qualified as K
import AlgST.Syntax.Name
import AlgST.Syntax.Traversal
import AlgST.Typing
import AlgST.Typing.Align
import AlgST.Util.PartialOrd
import Control.Monad.Reader.Class
import Test

positive :: (Example a) => a -> SpecWith (Arg a)
positive = specify "positive"

negative :: (Example a) => a -> SpecWith (Arg a)
negative = specify "negative"

spec :: Spec
spec = do
  describe "subtyping" do
    specify "equal types are subtypes" do
      let ?context = [("x", K.TU), ("y", K.SL), ("s", K.SL)]
      let check :: (HasCallStack) => String -> Assertion ()
          check s = s <=! s
      check "Int"
      check "Int -> Bool"
      check "Int -o Bool"
      check "(Int, Bool)"
      check "End!"
      check "x"
      check "Int -> x"
      check "x -> Int"
      check "x -> y"
      check "y -> x"
      check "forall a. a"
      check "forall a. x -> a"
      check "!Int.?Bool.s"

    specify "different type variables are not subtypes" do
      let ?context = [("x", K.TU), ("y", K.TU)]
      "x" </=! "y"

    specify "differing polarity inhibits subtyping" do
      let ?context = [("s", K.SL)]
      "End!" </=! "End?"
      "End?" </=! "End!"
      "!Int.s" </=! "?Int.s"
      "?Int.s" </=! "!Int.s"

    describe "pair subtyping" do
      let ?context = []
      positive do
        "(Int -> Bool, Int -> Bool)" <=! "(Int -o Bool, Int -> Bool)"
        "(Int -> Bool, Int -> Bool)" <=! "(Int -> Bool, Int -o Bool)"
        "(Int -> Bool, Int -> Bool)" <=! "(Int -o Bool, Int -o Bool)"
      negative do
        "(Int -o Bool, Int -> Bool)" </=! "(Int -> Bool, Int -> Bool)"
        "(Int -> Bool, Int -o Bool)" </=! "(Int -> Bool, Int -> Bool)"
        "(Int -o Bool, Int -o Bool)" </=! "(Int -> Bool, Int -> Bool)"

    describe "function return type covariance" do
      let ?context = []
      positive do
        "() -> (Int -> Bool)" <=! "() -> (Int -o Bool)"
      negative do
        "() -> (Int -o Bool)" </=! "() -> (Int -> Bool)"

    describe "function parameter contravariance" do
      let ?context = []
      positive do
        "(() -o Int) -o Int" <=! "(() -> Int) -o Int"
      negative do
        "(() -> Int) -o Int" </=! "(() -o Int) -o Int"

    describe "incoming messages covariance" do
      let ?context = [("s", K.SL)]
      positive do
        "?(Int -> Bool).s" <=! "?(Int -o Bool).s"
      negative do
        "?(Int -o Bool).s" </=! "?(Int -> Bool).s"

    describe "outgoing messages contravariance" do
      let ?context = [("s", K.SL)]
      positive do
        "!(Int -o Bool).s" <=! "!(Int -> Bool).s"
      negative do
        "!(Int -> Bool).s" </=! "!(Int -o Bool).s"

    describe "session continuation covariance" do
      let ?context = []
      positive do
        "!Int.!(Int -o Bool).End!" <=! "!Int.!(Int -> Bool).End!"
        "!Int.?(Int -> Bool).End!" <=! "!Int.?(Int -o Bool).End!"
      negative do
        "!Int.!(Int -> Bool).End!" </=! "!Int.!(Int -o Bool).End!"
        "!Int.?(Int -o Bool).End!" </=! "!Int.?(Int -> Bool).End!"

type HasContext = (?context :: [(String, K.Kind)])

-- | Asserts that the first argument is a subtype of the second argument.
(<=!) :: (HasCallStack, HasContext) => String -> String -> Assertion ()
t <=! u = do
  (t', u') <- synthTypes t u
  Alpha t' <=? Alpha u' @? unlines msg
  where
    msg =
      [ t,
        "  ---should be a subtype of---",
        u
      ]

-- | Asserts that the first argument is /not/ a subtype of the second argument.
(</=!) :: (HasCallStack, HasContext) => String -> String -> Assertion ()
t </=! u = do
  (t', u') <- synthTypes t u
  Alpha t' </=? Alpha u' @? unlines msg
  where
    msg =
      [ t,
        "  ---shouldn't be a subtype of---",
        u
      ]

-- | Parses, renames and kind-checks a pair of types.
synthTypes ::
  (HasCallStack, HasContext) => String -> String -> Assertion (TcType, TcType)
synthTypes t1 t2 = do
  t1Ty <- shouldParse parseType t1
  t2Ty <- shouldParse parseType t2
  withRenameContext do
    (rnNames, t1Rn, t2Rn) <- bind (mkPxy () @Parse @Rn) names \rnNames ->
      (rnNames,,) <$> renameSyntax t1Ty <*> renameSyntax t2Ty
    withTcContext \_ -> local (bindTcVars rnNames) do
      (,) <$> fmap fst (kisynth t1Rn) <*> fmap fst (kisynth t2Rn)
  where
    (varNames, kinds) = unzip ?context
    names :: [PName Types]
    names = UnqualifiedName . Unqualified <$> varNames
    bindTcVars :: (HasKiEnv env) => [RnName Types] -> env -> env
    bindTcVars rnNames = bindTyVars $ zip rnNames kinds
