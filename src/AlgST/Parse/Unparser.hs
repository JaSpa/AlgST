{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Unparsing (turning expressions and types back to their parsable string
-- forms) based on Norman Ramsey, Unparsing Expressions With Prefix and
-- Postfix Operators, Software—Practice and Experience, 1998.
-- https://www.cs.tufts.edu/~nr/pubs/unparse.ps
module AlgST.Parse.Unparser
  ( -- * The @Unparse@ class
    Unparse (..),
    Precedence (..),
    Op.Associativity (..),
    Fragment,
    Rator,

    -- * High-level unparsing operations
    unparseConst,
    unparseApp,
    unparseCase,
    showCaseMap,
    showForall,
  )
where

import AlgST.Builtins.Names (builtinOperators)
import AlgST.Syntax.Expression as E
import AlgST.Syntax.Kind qualified as K
import AlgST.Syntax.Name
import AlgST.Syntax.Operators qualified as Op
import AlgST.Syntax.Phases
import AlgST.Syntax.Type qualified as T
import Data.Bifunctor
import Data.Foldable
import Data.Functor.Identity
import Data.HashMap.Strict qualified as HM
import Data.List (intercalate, sortBy)
import Data.Map.Strict qualified as Map
import Data.Ord
import Data.Void

instance Show K.Multiplicity where
  show K.Un = "U"
  show K.Lin = "L"

showArrow :: K.Multiplicity -> String
showArrow K.Lin = " -o "
showArrow K.Un = " -> "

type Brackets = (Char, Char)

bracketsRound, bracketsSquare :: Brackets
bracketsRound = ('(', ')')
bracketsSquare = ('[', ']')

showSortedVar :: (Show a) => Brackets -> Name stage scope -> a -> String
showSortedVar (l, r) x t = [l] ++ pprName x ++ ":" ++ show t ++ [r]

showKind ::
  (Show a, Show b) =>
  (Brackets -> Name stage scope -> a -> String -> b -> String)
showKind brackets var sort arrow term =
  showSortedVar brackets var sort ++ arrow ++ show term

showForall :: (Unparse (T.XType x)) => K.Bind (XStage x) (T.Type x) -> String
showForall (K.Bind _ a k t) = "forall " ++ showKind bracketsRound a k ". " t

instance (Unparse (E.XExp x), Unparse (T.XType x)) => Show (E.Bind x) where
  show (E.Bind _ m x Nothing e) = pprName x ++ showArrow m ++ show e
  show (E.Bind _ m x (Just t) e) = showKind bracketsRound x t (showArrow m) e

data Precedence
  = PMin
  | -- | @in@, @else@, @case@, @rec@ (expressions)
    PIn
  | POpMin
  | POp Op.Precedence
  | -- | @λ … -> e@ in expressions,  @… -> …@ in types.
    PArrow
  | -- | @T1 . T2@
    PDot
  | -- | @dualof T@
    PDualof
  | -- | @e1 e2@ and @T1 T2@
    PApp
  | PMax
  deriving (Eq, Ord)

type Rator = (Precedence, Op.Associativity)

type Fragment = (Rator, String)

operatorRator :: ProgVar stage -> Maybe Rator
-- Before we have 'ResolvedName's we don't have the situation that we have to
-- render operators with the correct precedence and associativity due to
-- 'OperatorSequence' keeping information about how the user wrote it.
operatorRator op@ResolvedName {} =
  first POp <$> HM.lookup op builtinOperators
operatorRator _ =
  Nothing

minRator, inRator, opMinRator, dotRator, arrowRator, dualofRator, appRator, maxRator :: Rator
inRator = (PIn, Op.R)
opMinRator = (POpMin, Op.NA)
dotRator = (PDot, Op.R)
arrowRator = (PArrow, Op.R)
dualofRator = (PDualof, Op.R)
appRator = (PApp, Op.L)
minRator = (PMin, Op.NA)
maxRator = (PMax, Op.NA)

noparens :: Rator -> Rator -> Op.Associativity -> Bool
noparens (pi, ai) (po, ao) side = pi > po || pi == po && ai == ao && ao == side

bracket :: Fragment -> Op.Associativity -> Rator -> String
bracket (inner, image) side outer
  | noparens inner outer side = image
  | otherwise = "(" ++ image ++ ")"

class Unparse t where
  unparse :: t -> Fragment

instance Unparse Void where
  unparse = absurd

instance (Unparse a, Unparse b) => Unparse (Either a b) where
  unparse = either unparse unparse

unparseConst :: String -> Fragment
unparseConst s = unparseApp s ([] :: [Void])

unparseApp :: (Unparse a) => String -> [a] -> Fragment
unparseApp s = go (maxRator, s) . fmap unparse
  where
    go x [] = x
    go x (y : ys) =
      let l = bracket x Op.L appRator
          r = bracket y Op.R appRator
       in go (appRator, l ++ " " ++ r) ys

instance (Unparse (T.XType x)) => Show (T.Type x) where
  show = snd . unparse

instance (Unparse (T.XType x)) => Unparse (T.Type x) where
  unparse (T.Unit _) = (maxRator, "()")
  unparse (T.Var _ a) = (maxRator, pprName a)
  unparse (T.Con _ a Nothing) = (maxRator, pprName a)
  unparse (T.Con _ a (Just T.ProtocolSubset {..})) = do
    let complementFlag = if subsetComplement then "^" else ""
    let prettyCon x = (maxRator, pprName a ++ "[" ++ complementFlag ++ x ++ "]")
    let sortedConstructors = sortBy (comparing snd) $ Map.toList subsetConstructors
    if null subsetConstructors
      then prettyCon ""
      else prettyCon $ " " ++ intercalate ", " (pprName . fst <$> sortedConstructors) ++ " "
  unparse (T.Session _ p t u) = (dotRator, show p ++ t' ++ "." ++ u')
    where
      t' = bracket (unparse t) Op.L dotRator
      u' = bracket (unparse u) Op.R dotRator
  unparse (T.End _ p) = (maxRator, "End" ++ show p)
  unparse (T.Arrow _ m t u) = (arrowRator, l ++ showArrow m ++ r)
    where
      l = bracket (unparse t) Op.L arrowRator
      r = bracket (unparse u) Op.R arrowRator
  unparse (T.Pair _ t u) = (maxRator, "(" ++ l ++ ", " ++ r ++ ")")
    where
      l = bracket (unparse t) Op.L minRator
      r = bracket (unparse u) Op.R minRator
  unparse (T.Forall _ b) = (arrowRator, showForall b) -- ++ "=>" ++ s)
  unparse (T.Negate _ t) = (dualofRator, "-" ++ s)
    where
      s = bracket (unparse t) Op.R dualofRator
  unparse (T.Dualof _ t) = (dualofRator, "dual " ++ s)
    where
      s = bracket (unparse t) Op.R dualofRator
  unparse (T.App _ a b) = (appRator, l ++ " " ++ r)
    where
      l = bracket (unparse a) Op.L appRator
      r = bracket (unparse b) Op.R appRator
  unparse (T.Type x) = unparse x

instance (Unparse (E.XExp x), Unparse (T.XType x)) => Show (Exp x) where
  show = snd . unparse

instance (Unparse (E.XExp x), Unparse (T.XType x)) => Unparse (Exp x) where
  -- Basic values
  unparse (E.Lit _ l) = unparse l
  unparse (E.Var _ x) = (maxRator, pprName x)
  unparse (E.Con _ x) = (maxRator, pprName x)
  -- Abstraction intro and elim
  unparse (E.Abs _ b) = (arrowRator, '\\' : show b)
  unparse (E.App _ (E.App _ (E.Var _ x) e1) e2)
    | Just rator <- operatorRator x =
        let l = bracket (unparse e1) Op.L rator
            r = bracket (unparse e2) Op.R rator
         in (rator, l ++ " " ++ Op.pprRawName x ++ " " ++ r)
  unparse (E.App _ e1 e2) = (appRator, l ++ " " ++ r)
    where
      l = bracket (unparse e1) Op.L appRator
      r = bracket (unparse e2) Op.R appRator
  -- Pair intro and elim
  unparse (E.Pair _ e1 e2) = (maxRator, "(" ++ l ++ ", " ++ r ++ ")")
    where
      l = bracket (unparse e1) Op.L minRator
      r = bracket (unparse e2) Op.R minRator
  -- Datatype elim
  unparse (E.Case _ e m) = unparseCase e m
  -- Type Abstraction intro and elim
  unparse (E.TypeApp _ x t) =
    (appRator, show x ++ " [" ++ show t ++ "]")
  unparse (E.TypeAbs _ (K.Bind _ a k e)) =
    (arrowRator, '\\' : showKind bracketsSquare a k "->" e)
  -- Boolean elim
  unparse (E.Cond _ e1 e2 e3) =
    (inRator, "if " ++ s1 ++ " then " ++ s2 ++ " else " ++ s3)
    where
      s1 = bracket (unparse e1) Op.L inRator
      s2 = bracket (unparse e2) Op.NA inRator
      s3 = bracket (unparse e3) Op.R inRator
  -- Unary Let
  unparse (E.UnLet _ x Nothing (E.Rec _ x' ty e1) e2)
    | x == x' =
        (inRator, "let rec " ++ pprName x ++ " : " ++ show ty ++ " = " ++ l ++ " in " ++ r)
    where
      l = bracket (unparse (E.RecAbs e1)) Op.L inRator
      r = bracket (unparse e2) Op.R inRator
  unparse (E.UnLet _ x mty e1 e2) =
    (inRator, "let " ++ pprName x ++ annot ++ " = " ++ l ++ " in " ++ r)
    where
      annot = case mty of
        Nothing -> ""
        Just ty -> " : " ++ show ty
      l = bracket (unparse e1) Op.L inRator
      r = bracket (unparse e2) Op.R inRator
  unparse (E.PatLet _ x xs e1 e2) =
    (inRator, unwords s)
    where
      s =
        "let"
          : (pprName . unL <$> x : xs)
          ++ ["=", l, "in", r]
      l = bracket (unparse e1) Op.L inRator
      r = bracket (unparse e2) Op.R inRator
  unparse (E.Rec _ x ty r) =
    (inRator, "rec " ++ pprName x ++ " : " ++ show ty ++ " = " ++ show (E.RecAbs r))
  -- Session expressions
  unparse (E.New _ t) = (appRator, "new [" ++ show t ++ "]")
  unparse (E.Select _ (_ :@ con)) = (appRator, "select " ++ pprName con)
  -- Forking
  unparse (E.Fork _ e) = unparseApp "fork" [e]
  unparse (E.Fork_ _ e) = unparseApp "fork_" [e]
  -- Extensions
  unparse (E.Exp x) = unparse x

instance Unparse E.Lit where
  unparse =
    (maxRator,) . \case
      E.Unit -> "()"
      E.Int x -> show x
      E.Char x -> show x
      E.String x -> show x

instance (Unparse (E.XExp x), Unparse (T.XType x)) => Unparse (Op.OperatorSequence x) where
  unparse ops
    | Op.isSection ops = (maxRator, "(" ++ inner ++ ")")
    | otherwise = (opMinRator, inner)
    where
      inner = unwords . fmap bracketComponent . toList $ case ops of
        Op.OperandFirst _ ne -> ne
        Op.OperatorFirst _ ne -> ne
      bracketComponent e =
        bracket (unparse e) Op.NA arrowRator

unparseCase ::
  (Unparse (E.XExp x), Unparse (T.XType x), Foldable f, Foldable g) =>
  E.Exp x ->
  E.CaseMap' f g x ->
  Fragment
unparseCase e m =
  (inRator, "case " ++ s ++ " of {" ++ showCaseMap m ++ "}")
  where
    s = bracket (unparse e) Op.NA inRator

showCaseMap ::
  (Unparse (E.XExp x), Unparse (T.XType x), Foldable f, Foldable g) =>
  CaseMap' f g x ->
  String
showCaseMap m =
  intercalate ", " $
    map showAssoc (Map.toList (casesPatterns m))
      ++ [showWild b | b <- toList (casesWildcard m)]
  where
    showAssoc (c, CaseBranch {branchBinds = binds, branchExp = e}) =
      unwords (pprName . unL <$> ZeroPos :@ c : toList binds)
        ++ " -> "
        ++ show e
    showWild CaseBranch {branchBinds = Identity (_ :@ a), branchExp = e} =
      pprName a ++ " -> " ++ show e
