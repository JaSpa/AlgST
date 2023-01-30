{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module AlgST.Syntax.Tree
  ( LabTree,
    LabeledTree (..),
    drawLabeledTree,
    putLabeledTree,

    -- * Helpers & Re-exports
    Tree (..),
    tree,
    leaf,
    describeName,
    fieldMapTree,
  )
where

import AlgST.Parse.Unparser ()
import AlgST.Syntax.Decl qualified as D
import AlgST.Syntax.Expression qualified as E
import AlgST.Syntax.Kind qualified as K
import AlgST.Syntax.Module
import AlgST.Syntax.Name
import AlgST.Syntax.Operators
import AlgST.Syntax.Pos qualified as P
import AlgST.Syntax.Type qualified as T
import AlgST.Util.SourceLocation
import Control.Category ((>>>))
import Control.Monad
import Data.Foldable
import Data.Functor.Identity
import Data.HashMap.Strict qualified as HM
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe
import Data.Ord
import Data.Tree
import Data.Void

type LabTree = Tree String

class LabeledTree a where
  labeledTree :: a -> [LabTree]

drawLabeledTree :: (LabeledTree a) => a -> String
drawLabeledTree = labeledTree >>> drawForest

putLabeledTree :: (LabeledTree a) => a -> IO ()
putLabeledTree = drawLabeledTree >>> putStr

instance LabeledTree () where
  labeledTree _ = []

instance LabeledTree Void where
  labeledTree = absurd

instance (LabeledTree a) => LabeledTree (Maybe a) where
  labeledTree = concatMap labeledTree

instance (LabeledTree a, LabeledTree b) => LabeledTree (Either a b) where
  labeledTree = either labeledTree labeledTree

instance (LabeledTree a) => LabeledTree [a] where
  labeledTree = pure . tree "" . map labeledTree

-- | @Pos@ values are ignored. They are not part of the tree visualization.
instance LabeledTree P.Pos where
  labeledTree _ = []

instance LabeledTree SrcRange where
  labeledTree _ = []

instance (LabeledTree a) => LabeledTree (P.Located a) where
  labeledTree = labeledTree . P.unL

instance (LabeledTree a) => LabeledTree (Located a) where
  labeledTree = labeledTree . unL

instance LabeledTree K.Kind where
  labeledTree = pure . leaf . show

instance LabeledTree ModuleName where
  labeledTree = pure . leaf . unModuleName

instance LabeledTree (Name stage scope) where
  labeledTree = pure . leaf . describeName

describeName :: Name stage scope -> String
describeName n =
  pprName n ++ case n of
    ResolvedName {} -> '#' : pprResolved n
    _ -> ""

instance LabeledTree E.Lit where
  labeledTree =
    pure . leaf . \case
      E.Unit -> "Lit.Unit"
      E.Int x -> "Lit.Int " ++ show x
      E.Char x -> "Lit.Char " ++ show x
      E.String x -> "Lit.String " ++ show x

instance (T.ForallX LabeledTree x, E.ForallX LabeledTree x) => LabeledTree (E.Exp x) where
  labeledTree =
    pure . \case
      E.Lit x l ->
        tree "Exp.Lit" [labeledTree x, labeledTree l]
      E.Var x v ->
        tree ("Exp.Var " ++ describeName v) [labeledTree x]
      E.Con x v ->
        tree ("Exp.Con " ++ describeName v) [labeledTree x]
      E.Abs x1 (E.Bind x2 m v t e) ->
        let label = unwords ["Exp.Abs", show m, describeName v]
         in tree label [labeledTree x1, labeledTree x2, labeledTree t, labeledTree e]
      E.App x e1 e2 ->
        tree "Exp.App" [labeledTree x, labeledTree e1, labeledTree e2]
      E.Pair x e1 e2 ->
        tree "Exp.Pair" [labeledTree x, labeledTree e1, labeledTree e2]
      E.Cond x e1 e2 e3 ->
        tree "Exp.Cond" [labeledTree x, labeledTree e1, labeledTree e2, labeledTree e3]
      E.Case x e branches ->
        tree "Exp.Case" [labeledTree x, labeledTree e, fieldMapTree branches]
      E.TypeAbs x b ->
        let Node label ts = kbindNode "Exp.TypeAbs" b
         in Node label (labeledTree x ++ ts)
      E.TypeApp x e t ->
        tree "Exp.TypeApp" [labeledTree x, labeledTree e, labeledTree t]
      E.UnLet x v mty e1 e2 ->
        tree
          ("Exp.UnLet " ++ describeName v)
          [ labeledTree x,
            foldMap labeledTree mty,
            labeledTree e1,
            labeledTree e2
          ]
      E.PatLet x c vs e1 e2 ->
        tree
          ("Exp.PatLet " ++ unwords (describeName . P.unL <$> c : vs))
          [ labeledTree x,
            labeledTree e1,
            labeledTree e2
          ]
      E.Rec x v ty e ->
        tree
          ("Exp.Rec " ++ describeName v)
          [ labeledTree x,
            labeledTree ty,
            labeledTree (E.RecAbs e)
          ]
      E.New x t ->
        tree "Exp.New" [labeledTree x, labeledTree t]
      E.Select x (_ P.:@ c) ->
        tree ("Exp.Select " ++ describeName c) [labeledTree x]
      E.Fork x e ->
        tree "Exp.Fork" [labeledTree x, labeledTree e]
      E.Fork_ x e ->
        tree "Exp.Fork_" [labeledTree x, labeledTree e]
      E.Exp x ->
        tree "Exp.Exp" [labeledTree x]

instance
  (T.ForallX LabeledTree x, E.ForallX LabeledTree x, LabeledTree o) =>
  LabeledTree (OperatorSequence h x o)
  where
  labeledTree =
    pure
      . tree "OperatorSequence"
      . pure
      . foldOperatorSequence labeledTree labeledTree

instance
  (T.ForallX LabeledTree x, E.ForallX LabeledTree x, LabeledTree o) =>
  LabeledTree (SomeOperatorSequence x o)
  where
  labeledTree (SomeOperatorSequence ops) = labeledTree ops

instance (T.ForallX LabeledTree x) => LabeledTree (T.Type x) where
  labeledTree =
    pure . \case
      T.Unit x ->
        tree
          "Type.Unit"
          [labeledTree x]
      T.Arrow x m t1 t2 ->
        tree
          ("Type.Arrow " ++ show m)
          [labeledTree x, labeledTree t1, labeledTree t2]
      T.Pair x t1 t2 ->
        tree
          "Type.Pair"
          [labeledTree x, labeledTree t1, labeledTree t2]
      T.Session x p t1 t2 ->
        tree
          ("Type.Session (" ++ show p ++ ")")
          [labeledTree x, labeledTree t1, labeledTree t2]
      T.End x p ->
        tree
          ("Type.End" ++ show p)
          [labeledTree x]
      T.Forall x b ->
        let t = kbindNode "Type.Forall" b
         in t {subForest = labeledTree x ++ subForest t}
      T.Var x v ->
        let label = "Type.Var " ++ describeName v
         in tree label [labeledTree x]
      T.Con x v ps ->
        let label = "Type.Con " ++ describeName v
         in tree label [labeledTree x, labeledTree ps]
      T.App x t1 t2 ->
        tree
          "Type.App"
          [labeledTree x, labeledTree t1, labeledTree t2]
      T.Negate x t ->
        tree
          "Type.Negate"
          [labeledTree x, labeledTree t]
      T.Dualof x t ->
        tree
          "Type.Dualof"
          [labeledTree x, labeledTree t]
      T.Type x ->
        tree
          "Type.Type"
          [labeledTree x]

instance LabeledTree (T.ProtocolSubset x) where
  labeledTree T.ProtocolSubset {..} =
    [ tree
        ("Type.ProtocolSubset complement=" ++ show subsetComplement)
        [[leaf (describeName c)] | c <- Map.keys subsetConstructors]
    ]

instance (D.ForallDeclX LabeledTree x, T.ForallX LabeledTree x) => LabeledTree (D.TypeDecl x) where
  labeledTree =
    pure . \case
      D.AliasDecl x alias ->
        tree
          "AliasDecl"
          [ labeledTree x,
            typeAliasTree alias
          ]
      D.DataDecl x decl ->
        tree
          "DataDecl"
          [ labeledTree x,
            nominalDeclTree labeledTree decl
          ]
      D.ProtoDecl x decl ->
        tree
          "ProtoDecl"
          [ labeledTree x,
            nominalDeclTree labeledTree decl
          ]

instance (T.ForallX LabeledTree x) => LabeledTree (D.TypeAlias x) where
  labeledTree alias = pure $ tree "TypeAlias" [typeAliasTree alias]

typeAliasTree :: (T.ForallX LabeledTree x) => D.TypeAlias x -> [LabTree]
typeAliasTree D.TypeAlias {..} =
  [ Node "kind" [leaf (show aliasKind)],
    Node "parameters" (paramsTree aliasParams),
    Node "alias" $ labeledTree aliasType
  ]

nominalDeclTree :: (a -> [LabTree]) -> D.TypeNominal stage a -> [Tree [Char]]
nominalDeclTree f D.TypeNominal {..} =
  [ Node "kind" [leaf (show nominalKind)],
    Node "parameters" (paramsTree nominalParams),
    Node "constructors" $
      labeledMapTree (const . describeName) (\_ -> concatMap f . snd) nominalConstructors
  ]

paramsTree :: D.Params stage -> [LabTree]
paramsTree ps = [leaf $ "(" ++ describeName p ++ ":" ++ show k ++ ")" | _ :@ (p, k) <- ps]

instance (T.ForallX LabeledTree x) => LabeledTree (D.SignatureDecl x) where
  labeledTree D.SignatureDecl {signatureType = ty} =
    pure . Node "SignatureDecl" $
      [Node "type" $ labeledTree ty]

instance (T.ForallX LabeledTree x, E.ForallX LabeledTree x) => LabeledTree (D.ValueDecl x) where
  labeledTree vd =
    pure . Node "ValueDecl" $
      [ Node "type" $ labeledTree $ D.valueType vd,
        Node "params" $ leaf . param . unL <$> D.valueParams vd,
        Node "definition" $ labeledTree $ D.valueBody vd
      ]
    where
      param (Left tvar) = "[" ++ describeName tvar ++ "]"
      param (Right pvar) = describeName pvar

instance (D.ForallConX LabeledTree x, T.ForallX LabeledTree x) => LabeledTree (D.ConstructorDecl x) where
  labeledTree =
    pure . \case
      D.DataCon x parent params mul items ->
        tree
          ("Decl.DataCon (" ++ describeName parent ++ ")")
          [ labeledTree x,
            [Node "parameters" (paramsTree params)],
            [Node "multiplicity" [leaf (show mul)]],
            [tree "items" (labeledTree <$> items)]
          ]
      D.ProtocolCon x parent params items ->
        tree
          ("Decl.ProtocolCon (" ++ describeName parent ++ ")")
          [ labeledTree x,
            [Node "parameters" (paramsTree params)],
            [tree "items" (labeledTree <$> items)]
          ]

instance (ForallX LabeledTree x) => LabeledTree (Module x) where
  labeledTree pp = types ++ sigs ++ values ++ (showBench <$> moduleBench pp)
    where
      types =
        labeledMapTree
          (\tv _ -> describeName tv)
          (\_ td -> labeledTree td)
          (moduleTypes pp)
      sigs =
        labeledMapTree
          (\pv _ -> describeName pv)
          (\_ sig -> labeledTree sig)
          (moduleSigs pp)
      values =
        labeledMapTree
          (\pv _ -> describeName pv)
          (\_ d -> either labeledTree labeledTree d)
          (moduleValues pp)
      showBench bench =
        tree
          (benchPrefix bench ++ show (benchName bench))
          [ labeledTree (benchT1 bench),
            labeledTree (benchT2 bench)
          ]
      benchPrefix b
        | benchExpect b = "benchmark "
        | otherwise = "benchmark! "

instance (LabeledTree a) => LabeledTree (Import a) where
  labeledTree i =
    [ tree
        ("Import " ++ "unqualified" `fromMaybe` qualified)
        [ [tree "target" [labeledTree (importTarget i)]],
          labeledTree (importSelection i)
        ]
    ]
    where
      qualified = do
        guard $ emptyModuleName /= importQualifier i
        Just $ "as " ++ unModuleName (importQualifier i)

instance LabeledTree ImportSelection where
  labeledTree =
    pure . \case
      ImportAll _ hiddenSet renamedMap ->
        tree "ImportAll" [hidden hiddenSet, renamed renamedMap]
      ImportOnly useMap ->
        tree "ImportOnly" [renamed useMap]
    where
      hidden =
        HM.keys >>> List.sort >>> fmap \(s, n) ->
          leaf $ unwords ["hide", scope s, name n]
      renamed =
        HM.toList >>> List.sortBy (comparing fst) >>> fmap \((s, n), _ :@ o) ->
          leaf $ unwords ["use", scope s, name o, "as", name n]
      scope = \case
        Values -> "value"
        Types -> "type"
      name (Unqualified x) =
        "‘" ++ x ++ "’"

labeledMapTree ::
  (a -> b -> String) ->
  (a -> b -> [LabTree]) ->
  Map.Map a b ->
  [LabTree]
labeledMapTree f g = fmap (\(a, b) -> Node (f a b) (g a b)) . Map.toList

kbindNode :: (LabeledTree a) => String -> K.Bind stage a -> LabTree
kbindNode con (K.Bind _ v k a) =
  let label = unwords [con, describeName v ++ ":" ++ show k]
   in Node label $ labeledTree a

fieldMapTree ::
  (T.ForallX LabeledTree x, E.ForallX LabeledTree x, Foldable f, Foldable g) =>
  E.CaseMap' f g x ->
  [LabTree]
fieldMapTree m = conCases ++ wildCases
  where
    conCases =
      labeledMapTree
        (\v b -> unwords [describeName x | _ P.:@ x <- P.ZeroPos P.:@ v : toList (E.branchBinds b)])
        (\_ b -> labeledTree $ E.branchExp b)
        (E.casesPatterns m)
    wildCases =
      [ Node (describeName x) (labeledTree e)
      | E.CaseBranch
          { branchBinds = Identity (_ P.:@ x),
            branchExp = e
          } <-
          toList (E.casesWildcard m)
      ]

leaf :: a -> Tree a
leaf a = Node a []

tree :: a -> [[Tree a]] -> Tree a
tree a = Node a . concat
