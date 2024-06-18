{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
module AlgST.Parse.Parser
  ( -- * Parsers
    Parser
  , runParser
  , parseModule
  , parseDecls
  , parseImports
  , parseType
  , parseKind
  , parseExpr

    -- * Re-exports
  , ParsedModule(..)
  , emptyParsedModule
  , resolveImports
  ) where

import           Control.Category              ((>>>), (<<<))
import qualified Control.Foldl                 as L
import qualified Control.Foldl.NonEmpty        as L1
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Bifunctor
import qualified Data.DList                    as DL
import qualified Data.DList.DNonEmpty          (DNonEmpty)
import qualified Data.DList.DNonEmpty          as DNE
import           Data.Either
import           Data.Foldable
import           Data.Functor
import           Data.Functor.Identity
import qualified Data.List                     as List
import           Data.List.NonEmpty            (NonEmpty(..))
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Profunctor
import           Data.Proxy
import           Data.Semigroup                (Semigroup(..))
import           Data.Sequence                 (Seq(..))
import qualified Data.Sequence                 as Seq
import           Lens.Family2
import           AlgST.Builtins.Names
import           AlgST.Parse.ParseUtils
import           AlgST.Parse.Phase
import           AlgST.Parse.Token
import           AlgST.Syntax.Decl
import           AlgST.Syntax.Expression       as E
import qualified AlgST.Syntax.Kind             as K
import           AlgST.Syntax.Module
import           AlgST.Syntax.Name
import           AlgST.Syntax.Operators
import qualified AlgST.Syntax.Type             as T
import           AlgST.Util
import           AlgST.Util.SourceLocation
}

%name parseModule_  Module
%name parseImports_ Imports
%name parseDecls_   Decls
%name parseKind_    Kind
%name parseType_    Type
%name parseExpr_    Exp

%tokentype { Token }
%error { errorUnexpectedTokens }
%monad { Parser }
%lexer { scanToken } { TokenEof _ }

%token
  nl       { TokenNL        _ }
  '()'     { TokenUnit      _ }
  '->'     { TokenUnArrow   _ }
  '-o'     { TokenLinArrow  _ }
  lambda   { TokenLambda    _ }
  '('      { TokenLParen    _ }
  ')'      { TokenRParen    _ }
  ','      { TokenComma     _ }
  '['      { TokenLBracket  _ }
  ']'      { TokenRBracket  _ }
  ':'      { TokenColon     _ }
  '!'      { TokenMOut      _ }
  '?'      { TokenMIn       _ }
  '{'      { TokenLBrace    _ }
  '}'      { TokenRBrace    _ }
  '_'      { TokenWild      _ }
  '.'      { TokenDot       _ }
  '(,)'    { TokenPairCon   _ }
  '{-#'    { TokenLPragma   _ }
  '#-}'    { TokenRPragma   _ }

  -- Identifiers. 'as' and '(*)' can appear as special syntax items.
  as        { TokenLowerId  ($$ :@ "as")  }
  '(*)'     { TokenLowerId  ($$ :@ "(*)") }
  LOWER_ID  { TokenLowerId  $$ }
  UPPER_ID  { TokenUpperId  $$ }

  -- Operators. +/-/*/^ can appear as special syntax items.
  '+'      { TokenOperator ($$ :@ "+") }
  '-'      { TokenOperator ($$ :@ "-") }
  '*'      { TokenOperator ($$ :@ "*") }
  '^'      { TokenOperator ($$ :@ "^") }
  OPERATOR { TokenOperator $$ }

  INT      { TokenInt      $$ }
  CHAR     { TokenChar     $$ }
  STR      { TokenString   $$ }
  rec      { TokenRec      _  }
  let      { TokenLet      _  }
  in       { TokenIn       _  }
  '='      { TokenEq       _  }
  data     { TokenData     _  }
  protocol { TokenProtocol _  }
  type     { TokenType     _  }
  '|'      { TokenPipe     _  }
  if       { TokenIf       _  }
  then     { TokenThen     _  }
  else     { TokenElse     _  }
  new      { TokenNew      _  }
  select   { TokenSelect   _  }
  fork     { TokenFork     _  }
  fork_    { TokenFork_    _  }
  case     { TokenCase     _  }
  of       { TokenOf       _  }
  forall   { TokenForall   _  }
  dualof   { TokenDualof   _  }
  end      { TokenEnd      $$ }
  import   { TokenImport   _  }

%%

-------------
-- MODULES --
-------------

Module :: { ParsedModule }
  : {- empty -}           {  ParsedModule [] emptyModule }
  | TopItems              {% ParsedModule [] `fmap` runModuleBuilder $1 }
  | Imports               {  ParsedModule $1 emptyModule }
  | Imports nl TopItems   {% ParsedModule $1 `fmap` runModuleBuilder $3 }

TopItems :: { ModuleBuilder }
  : TopItem             { $1 }
  | TopItems nl TopItem { $1 >>> $3 }

TopItem :: { ModuleBuilder }
  : Decl              { $1 }
  | Pragma            { $1 }

-------------------------------------------------------------------------------
-- Imports
-------------------------------------------------------------------------------

Imports :: { [Located (Import ModuleName)] }
  : Import              { [$1] }
  | Imports nl Import   { $3 : $1 }

Import :: { Located (Import ModuleName) }
  : import ModuleName ImportList {% do
      selection <- $3 $! getRange $1
      pure $ $1 @- Import {
        importTarget = unL $2,
        importQualifier = emptyModuleName,
        importSelection = selection
      }
    }

-- Parses an import list. The given `SrcRange` should be the range of the
-- import statement without the import list, ie. the range of the `import`
-- token and the imported module.
ImportList :: { SrcRange -> Parser ImportSelection }
  -- The optional `nl` tokens allow the closing parenthesis to appear on a new
  -- line in column 0.
  : {- empty -}                       { \ir -> pure $ ImportAll ir mempty mempty }
  | '(*)'                             { const $ pure $ ImportAll (getRange $1) mempty mempty }
  | '()'                              { const $ pure $ ImportOnly mempty }
  | '(' opt(nl) ')'                   { const $ pure $ ImportOnly mempty }
  -- Extend the range to include the closing parenthesis (and with this the
  -- whole import list).
  | '(' ImportSelection opt(nl) ')'   { ($2 $!) . runion $4 }

ImportSelection :: { SrcRange -> Parser ImportSelection }
  : ImportItems opt(',')
    { \stmtRange -> mergeImportOnly stmtRange (DL.toList $1) }
  | '*' ',' ImportItems opt(',')
    { \stmtRange -> mergeImportAll stmtRange (getRange $1) (DL.toList $3) }
  | '*' opt(',')
    { \_ -> pure $ ImportAll $1 mempty mempty }

ImportItems :: { DL.DList ImportItem }
  : ImportItem                        { DL.singleton $1 }
  | ImportItems ',' ImportItem        { $1 `DL.snoc` $3 }

ImportItem :: { ImportItem }
  : ImportScope UnqualifiedVar
    { mkImportItem $1 $2 ImportAsIs }
  | ImportScope UnqualifiedCon
    { mkImportItem $1 $2 ImportAsIs }
  | ImportScope UnqualifiedVar as '_'
    { mkImportItem $1 $2 ImportHide }
  | ImportScope UnqualifiedCon as '_'
    { mkImportItem $1 $2 ImportHide }
  | ImportScope UnqualifiedVar as UnqualifiedVar
    { mkImportItem $1 $4 (ImportFrom (unL $2)) }
  | ImportScope UnqualifiedCon as UnqualifiedCon
    { mkImportItem $1 $4 (ImportFrom (unL $2)) }

ImportScope :: { SrcRange -> Located Scope }
  : {- empty -}   { \p -> p  @- Values }
  | type          { \_ -> $1 @- Types }

-------------------------------------------------------------------------------
-- Pragmas
-------------------------------------------------------------------------------

PragmaBenchmark :: { SrcRange -> ModuleBuilder }
  : UPPER_ID { \pragmaRange -> liftModuleBuilder do
      when (unL $1 /= "BENCHMARK") do
        addErrors [errorUnknownPragma pragmaRange (getRange $1)]
    }

Pragma :: { ModuleBuilder }
  : '{-#' PragmaBenchmark opt('!') opt(STR) TypeAtom opt(nl) TypeAtom opt(nl) '#-}' {
      $2 ($1 `runion` $9) >>> insertBenchmark Benchmark
        { benchName   = foldMap unL $4
        , benchExpect = isNothing $3
        , benchT1     = $5
        , benchT2     = $7
        }
    }
  | '{-#' PragmaBenchmark opt('!') opt(STR) nl Type nl Type opt(nl) '#-}' {
      $2 ($1 `runion` $10) >>> insertBenchmark Benchmark
        { benchName   = foldMap unL $4
        , benchExpect = isNothing $3
        , benchT1     = $6
        , benchT2     = $8
        }
    }

-------------------------------------------------------------------------------
-- Declarations
-------------------------------------------------------------------------------

Decls :: { ModuleBuilder }
  : Decl          { $1 }
  | Decls nl Decl { $1 >>> $3 }

Decl :: { ModuleBuilder }
  -- Function signature
  : ProgVar TySig {
      moduleValueDecl $1 $2
    }
  -- Function declaration
  | ProgVar ValueParams '=' Exp {
      moduleValueBinding $1 $2 $4
    }
  -- Type abbreviation
  | type KindedTVar TypeParams '=' Type { do
      let (_nameRange, name, mkind) = $2
      let !range = sconcat $ getRange $1 :| fmap getRange $3 
      let decl = AliasDecl range TypeAlias
            { aliasParams = $3
            , aliasKind = mkind
            , aliasType = $5
            }
      moduleTypeDecl name decl
    }
  -- Datatype declaration
  | data KindedTVar TypeParams { do
      let (_nameRange, name, mkind) = $2
      let !range = sconcat $ getRange $1 :| fmap getRange $3 
      let decl = DataDecl range TypeNominal
            { nominalParams = $3
            , nominalKind = K.TU `fromMaybe` mkind
            , nominalConstructors = mempty
            }
      moduleTypeDecl name decl
    }
  | data KindedTVar TypeParams '=' DataCons { do
      let (_nameRange, name, mkind) = $2
      let !range = sconcat $ getRange $1 :| fmap getRange $3 
      let decl = DataDecl range TypeNominal
            { nominalParams = $3
            , nominalKind = K.TU `fromMaybe` mkind
            , nominalConstructors = $5
            }
      moduleTypeDecl name decl
    }
  | protocol KindedTVar TypeParams '=' DataCons { do
      let (_nameRange, name, mkind) = $2
      let !range = sconcat $ getRange $1 :| fmap getRange $3 
      let decl = ProtoDecl range TypeNominal
            { nominalParams = $3
            , nominalKind = K.P `fromMaybe` mkind
            , nominalConstructors = $5
            }
      moduleTypeDecl name decl
    }

TySig :: { PType }
  : ':' Type     { $2 }

TypeParams :: { Params PStage }
  : {- empty -}   { [] }
  | TypeParams1   { toList $1 }

-- A `forall` requires a non-empty list of type var bindings.
TypeParams1 :: { NonEmpty (Located (TypeVar PStage, K.Kind)) }
  : bindings1(KindBind) {% $1 \(r :@ (name, _)) -> Identity (r :@ name) }

DataCons :: { Constructors PStage PType }
  : DataCon              {  uncurry Map.singleton $1 }
  | '|' DataCon          {  uncurry Map.singleton $2 }
  | DataCons '|' DataCon {% uncurry insertNoDuplicates $3 $1 }

DataCon :: { (ProgVar PStage, (SrcRange, [PType])) }
  : Constructor TypeSeq { (unL $1, (getRange $1, DL.toList $2)) }

ValueParams :: { [Located AName] }
  : bindings(ValueParam) {%
      let isAWildcard = either isWild isWild . unL
       in concat `fmap` $1 (filter (not . isAWildcard))
    }

ValueParam :: { [Located AName] }
  : ProgVarWild         { [fmap liftName $1] }
  | '[' TyVarList ']'   { DL.toList $2 }

TyVarList :: { DL.DList (Located AName) }
  : wildcard(TypeVar)                       { DL.singleton (fmap liftName $1) }
  | TyVarList ',' wildcard(TypeVar)         { $1 `DL.snoc` fmap liftName $3 }


-------------------------------------------------------------------------------
-- Expressions
-------------------------------------------------------------------------------

EAtom :: { PExp }
  : INT                            { E.Lit (getRange $1) $ E.Int    (unL $1) }
  | CHAR                           { E.Lit (getRange $1) $ E.Char   (unL $1) }
  | STR                            { E.Lit (getRange $1) $ E.String (unL $1) }
  | '()'                           { E.Lit (getRange $1) E.Unit }
  | '(,)'                          {% fatalError $ errorMisplacedPairCon @Values (getRange $1) }
  | ProgVar                        { uncurryL E.Var $1 }
  | Constructor                    { uncurryL E.Con $1 }
  | '(' ExpInner ')'               {% $2 $ InParens $ $1 `runion` $3 }
  | '(' Exp ',' Exp ')'            { E.Pair ($1 `runion` $5) $2 $4 }
  | case Exp of Cases              { E.Case ($1 `runion` $4) $2 (unL $4) }
  | new                            { E.Exp $ Left $ BuiltinNew   $ getRange $1 }
  | fork                           { E.Exp $ Left $ BuiltinFork  $ getRange $1 }
  | fork_                          { E.Exp $ Left $ BuiltinFork_ $ getRange $1 }

ETail :: { PExp }
  : LamExp                         { $1 }
  | if Exp then Exp else Exp       { E.Cond ($1 `runion` $6) $2 $4 $6 }
  | let LetBind '=' Exp in Exp     { $2 ($1 `runion` $6) $4 $6 }
  | RecExp                         { snd $1 }
  | let RecExp in Exp              { do
      let (v, r) = $2
      E.UnLet ($1 `runion` $4) v Nothing r $4
    }

EApp :: { PExp }
  : EAtom                          { $1 }
  | EApp EAtom                     { E.App ($1 `runion` $2) $1 $2 }
  | EApp '[' TypeApps ']'          { E.foldTypeApps (\_ _ -> $1 `runion` $4) $1 $3 }
  | select Constructor             { E.Select ($1 `runion` $2) $2 }
  | select '(,)'                   { E.Select ($1 `runion` $2) ($2 @- PairCon) }

EAppTail :: { PExp }
  : EApp                           { $1 }
  | ETail                          { $1 }
  | EApp ETail                     { E.App ($1 `runion` $2) $1 $2 }

EOps :: { Parenthesized -> Parser PExp }
  : OpTys
    { \p -> do
        -- A single operator may be used as a function value if it is wrapped in
        -- parentheses. Without parentheses we will complain.
        when (p == TopLevel) $ void do
          addError $ errorMissingOperand Nothing $1
        pure $1
    }
  | OpsExp
    { \ps -> sectionsParenthesized ps $1 }
  | OpTys EAppTail
    { \ps -> sectionsParenthesized ps $ opsLeftSection $1 $2 }
  | OpTys OpsExp
    { \ps -> sectionsParenthesized ps $ $1 `consOperator` $2 }

ExpInner :: { Parenthesized -> Parser PExp }
  : EOps                           { $1 }
  | EAppTail                       { const (pure $1) }

Exp :: { PExp }
  : ExpInner                       {% $1 TopLevel }

TypeApps :: { DL.DList PType }
  : Type                           { DL.singleton $1 }
  | TypeApps ',' Type              { $1 `DL.snoc` $3 }

-- Parses a rec-expression and retuns the recursive name alongside.
RecExp :: { (ProgVar PStage, PExp) }
  : rec ProgVar TySig '=' Exp {% do
      let fullRange = $1 `runion` $5
      case $5 of
        E.RecAbs r -> pure (unL $2, E.Rec fullRange (unL $2) $3 r)
        _ -> fatalError $ errorRecBadRhsLambda fullRange (getRange $1) $5
    }

-- Constructs a let-expression from a
--
--   * range (full expression range)
--   * bound expression
--   * continuation expression
LetBind :: { SrcRange -> PExp -> PExp -> PExp }
  : ProgVarWild opt(TySig)        { \r -> E.UnLet r (unL $1) $2 }
  | Pattern                       { \r -> uncurry (E.PatLet r) $1 }

LamExp :: { PExp }
  : lambda Abs Arrow Exp {% do
      let (build, anyTermAbs, absRange) = $2
      let arrRange :@ arrMul = $3
      when (arrMul == K.Lin && not anyTermAbs) do
        addErrors [errorNoTermLinLambda ($1 `runion` absRange) arrRange]
      pure $ appEndo (build (getRange $1) arrMul) $4
    }

-- Parses a non empty list of bindings for a lambda abstraction. Returns
--
-- 1. A function to build a nested abstraction from the arguments
--      (a) location of the lambda
--          TODO: rethink this when the AST has proper ranges!
--      (b) multiplicity of the lambda
-- 2. A Bool indicating if there were any actual value bindings
-- 3. A range containing all the bindings
Abs :: { (SrcRange -> K.Multiplicity -> Endo PExp, Bool, SrcRange) }
  : bindings1(Abs1) {% do
      binds <- $1 $ \case
        p :@ Left (v, _)  | not (isWild v) -> Just (p @- Left v)
        p :@ Right (v, _) | not (isWild v) -> Just (p @- Right v)
        _ -> Nothing
      let mkVAbs argRange (v, t) lamLoc m =
            Endo $ E.Abs @Parse lamLoc . E.Bind argRange m v t
      let mkTAbs argRange (v, k) lamLoc _ =
            Endo $ E.TypeAbs @Parse lamLoc . K.Bind argRange v k
      let f1 = pure (,,)
            <*> lmap (\(r :@ arg) -> either (mkVAbs r) (mkTAbs r) arg) L1.sconcat
            <*> L1.fromFold (L.any (isLeft . unL))
            <*> lmap getRange L1.sconcat 
      pure $ L1.fold1 f1 binds
    }

Abs1 :: { Located (Either (ProgVar PStage, Maybe PType) (TypeVar PStage, K.Kind)) }
  : wildcard(ProgVar)                  { $1 @- Left (unL $1, Nothing) }
  | '(' wildcard(ProgVar) ')'          { ($1 `runion` $3) @- Left (unL $2, Nothing) }
  | '(' wildcard(ProgVar) ':' Type ')' { ($1 `runion` $5) @- Left (unL $2, Just $4) }
  | '[' wildcard(TypeVar) ':' Kind ']' { ($1 `runion` $5) @- Right (unL $2, unL $4) }

Cases :: { Located PCaseMap }
  : -- An empty case is not allowed. Accepting it here allows us to provide
    -- better error messages.
    '{' '}'                            { ($1 `runion` $2) @- emptyCaseMap }
  | -- optional nl: The closing brace may be on column 0 of a new line. Usually
    -- nl separates declarations.
    '{' CaseMap opt(',') opt(nl) '}'   { ($1 `runion` $5) @- $2 }

CaseMap :: { PCaseMap }
  : Case             {% $1 emptyCaseMap }
  | CaseMap ',' Case {% $3 $1 }

Case :: { PCaseMap -> Parser PCaseMap }
  : Pattern '->' Exp { \pcm -> do
      let (con, binds) = $1
      let branch = CaseBranch
            { branchRange = con `runion` $3
            , branchBinds = binds
            , branchExp = $3
            }
      cases <- insertNoDuplicates (unL con) branch (E.casesPatterns pcm)
      pure pcm{ E.casesPatterns = cases }
    }
  | ProgVarWild '->' Exp { \pcm -> do
      let wildBranch = CaseBranch
            { branchRange = $1 `runion` $3
            , branchBinds = Identity $1
            , branchExp = $3
            }
      whenJust (E.casesWildcard pcm) \prevWild ->
        addErrors [errorMultipleWildcards prevWild wildBranch]
      pure pcm{ E.casesWildcard = Just wildBranch }
    }

Pattern :: { (Located (ProgVar PStage), [Located (ProgVar PStage)]) }
  : Constructor ProgVarWildSeq            { ($1, $2) }
  | '(,)' ProgVarWildSeq                  { ($1 @- PairCon, $2) }
  | '(' ProgVarWild ',' ProgVarWild ')'   {% do
      when (onUnL (==) $2 $4 && not (foldL isWild $2)) do
        addErrors [errorDuplicateBind (getRange $2) (getRange $4)]
      pure ($1 @- PairCon, [$2, $4])
    }

ProgVarWildSeq :: { [Located (ProgVar PStage)] }
  : bindings(ProgVarWild)   {% $1 \v ->
      -- Only check for duplicates if it is not a wildcard.
      if isWild (unL v)
      then Nothing
      else Just v
    }

Op :: { Located (ProgVar PStage) }
  : OPERATOR  { $1 @- UnqualifiedName (Unqualified ("(" ++ unL $1 ++ ")")) }
  | '+'       { $1 @- UnqualifiedName (Unqualified "(+)") }
  | '-'       { $1 @- UnqualifiedName (Unqualified "(-)") }
  | '*'       { $1 @- UnqualifiedName (Unqualified "(*)") }
  | '^'       { $1 @- UnqualifiedName (Unqualified "(^)") }

OpTys :: { PExp }
  : Op                      { uncurryL E.Var $1 }
  | OpTys '[' TypeApps ']'  { E.foldTypeApps (\_ _ -> $1 `runion` $4) $1 $3 }

OpsExp :: { OperatorSequence HeadE Parse PExp }
  : EApp OpTys              { opsRightSection $1 $2 }
  | EApp OpTys EAppTail     { opsSimpleSeq $1 $2 $3 }
  | EApp OpTys OpsExp       { $1 `consOperand` $2 `consOperator` $3 }


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- polarised(t :: PType) :: PType
polarised(t)
  :     t              { $1 }
  | '+' polarised(t)   { $2 }
  | '-' polarised(t)   { T.Negate ($1 `runion` $2) $2 :: PType }

ConSequence1 :: { NameMap Values SrcRange }
  : Constructor                   { Map.singleton (unL $1) (getRange $1) }
  | ConSequence1 ',' Constructor  { Map.insert (unL $3) (getRange $3) $1 }

ConSequence :: { NameMap Values SrcRange }
  : {- empty -}                   { Map.empty }
  | ConSequence1                  { $1 }
  | ConSequence1 ','              { $1 }

ProtocolSubset :: { T.ProtocolSubset Written }
  : '[' opt('^') ConSequence ']' { T.ProtocolSubset {
      T.subsetComplement = isJust $2,
      T.subsetConstructors = $3
    } }

TypeAtom :: { PType }
  : '()'                          { T.Unit (getRange $1) }
  | '(,)'                         {% fatalError $ errorMisplacedPairCon @Types (getRange $1) }
  | '(' Type ',' TupleType ')'    { T.Pair (getRange $1) $2 $4 }
  | end                           { uncurryL T.End $1 }
  | TypeVar                       { uncurryL T.Var $1 }
  | TypeName opt(ProtocolSubset)  { uncurryL T.Con $1 $2 }
  | '(' Type ')'                  { $2 }

Type1 :: { PType }
  : TypeAtom                      { $1 }
  | Type1 polarised(TypeAtom)     { T.App ($1 `runion` $2) $1 $2 }

Type2 :: { PType }
  : polarised(Type1)              { $1 }

Type3 :: { PType }
  : Type2                         { $1 }
  | Polarity Type2 '.' Type3      { do
      let pol = $1 & rangeL %~ \r -> r `runion` $2 `runion` $3
      uncurryL T.Session pol $2 $4
    }

Type4 :: { PType }
  : Type3                         { $1 }
  | dualof Type4                  { T.Dualof ($1 `runion` $2) $2 }

Type5 :: { PType }
  : Type4                         { $1 }
  | Type4 Arrow Type5             { T.Arrow ($1 `runion` $3) (unL $2) $1 $3 }
  | Forall Type5                  { $1 $2 }

Type :: { PType }
  : Type5                         { $1 }

Forall :: { PType -> PType }
  : forall TypeParams1 '.' { \t -> do
      let !range = $1 `runion` $3 `runion` t
      let bind (r :@ (v, k)) = Endo $ T.Forall @Parse range . K.Bind r v k
      appEndo (foldMap bind $2) t
    }

TupleType :: { PType }
  : Type               { $1 }
  | Type ',' TupleType { T.Pair ($1 `runion` $3) $1 $3 }

Arrow :: { Located K.Multiplicity }
  : '->' { $1 @- K.Un }
  | '-o' { $1 @- K.Lin }

Polarity :: { Located T.Polarity }
  : '!' { $1 @- T.Out }
  | '?' { $1 @- T.In }

-- A sequence of types to be used in a constructor declaration.
TypeSeq :: { DL.DList PType }
  : {- empty -}                     { DL.empty }
  | TypeSeq polarised(TypeAtom)     {  $1 `DL.snoc` $2 }
    -- TypeAtom by itself does not allow prefixing with +/- without wrapping in
    -- parentheses. The polarisation also can't be moved to TypeAtom since it
    -- has a lower precedence than type application


-------------------------------------------------------------------------------
-- Kinds
-------------------------------------------------------------------------------

Kind :: { Located K.Kind }
  : UPPER_ID {%
      case reads (unL $1) of
        [(k, "")] -> pure $ $1 @- k
        _ -> $1 @- K.TU <$ addErrors [errorInvalidKind (getRange $1)]
    }


-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

UnqualifiedCon :: { Located Unqualified }
  : UPPER_ID        { $1 @- Unqualified (unL $1) }

UnqualifiedVar :: { Located Unqualified }
  : LOWER_ID        { $1 @- Unqualified (unL $1) }
  | -- (*) is special syntax in import lists.
    '(*)'           { $1 @- Unqualified "(*)" }
  | -- 'as' is a contextual keyword.
    as              { $1 @- Unqualified "as" }

-- Do to a clash with session type syntax we can't parse module names in the
-- lexer.
-- Note: I guess we could but this would require more work as we would have to
-- introduce start codes.
-- See https://www.jyotirmoy.net/posts/2015-08-17-alex-happy-startcodes.html
-- if this comes into focus again.
ModuleName :: { Located ModuleName }
  : ModuleNameRec
    { fmap (ModuleName . List.intercalate "." . DL.toList) $1 }

ModuleNameRec :: { Located (DL.DList String) }
  : UnqualifiedCon
    { fmap (DL.singleton . getUnqualified) $1 }
  | ModuleNameRec '.' UnqualifiedCon
    { fmap (`DL.snoc` getUnqualified (unL $3)) $1 }

NameVar :: { Located UnscopedName }
  : UnqualifiedVar  { $1 <&> \u -> UName (UnqualifiedName u) }

NameCon :: { Located UnscopedName }
  : UnqualifiedCon  { $1 <&> \u -> UName (UnqualifiedName u) }

ProgVar :: { Located (ProgVar PStage) }
  : NameVar { scopeName `fmap` $1 }

Constructor :: { Located (ProgVar PStage) }
  : NameCon { scopeName `fmap` $1 }

ProgVarWild :: { Located (ProgVar PStage) }
  : wildcard(ProgVar)   { $1 }

TypeVar :: { Located (TypeVar PStage) }
  : NameVar { scopeName `fmap` $1 }

TypeName :: { Located (TypeVar PStage) }
  : NameCon { scopeName `fmap` $1 }

KindBind :: { Located (TypeVar PStage, K.Kind) }
  : '(' TypeVar ':' Kind ')'  { ($1 `runion` $5) @- (unL $2, unL $4) }
  | '(' TypeVar ')'           { ($1 `runion` $3) @- (unL $2, K.TU) }
  | TypeVar                   { $1 @- (unL $1, K.TU) }

KindedTVar :: { (SrcRange, TypeVar PStage, Maybe K.Kind) }
  : TypeName ':' Kind { ($1 `runion` $3, unL $1, Just (unL $3)) }
  | TypeName          { (getRange $1, unL $1, Nothing) }

-- wildcard(v) : v ~ Located s => Located (Name s)
wildcard(v)
  : v     { $1 }
  | '_'   { $1 @- Wildcard }

-- bindings(p) :
--   (Foldable f, Eq a, Hashable a, ErrorMsg a) => (p -> f (Located a)) -> Parser [p]
--
-- Parses a sequence of `p` and ensures that the extracted `a`s are different.
bindings(p)
  : bindings_(p) { fmap DL.toList . $1 Map.empty }

-- bindings1(p) :
--   (Foldable f, Eq a, Hashable a, ErrorMsg a) => (p -> f (Located a)) -> Parser (NonEmpty p)
--
-- Like `bindings` but for a non-empty sequence.
bindings1(p)
  : p bindings_(p) { \(extractAs :: p -> f (Located a)) -> do
      bound <- foldM
        (flip \(p :@ a) -> insertNoDuplicates a p)
        Map.empty
        (extractAs $1)
      ps <- $2 bound extractAs
      pure $ $1 :| DL.toList ps
    }

bindings_(p)
  : {- empty -} { \_ _ ->
      pure $ DL.empty
    }
  | bindings_(p) p { \bound extractAs -> do
      bound' <- foldM
        (flip \(p :@ a) -> insertNoDuplicates a p)
        bound
        (extractAs $2)
      ps <- $1 bound' extractAs
      pure $ ps `DL.snoc` $2
    }


-------------------------------------------------------------------------------
-- Generic Helpers
-------------------------------------------------------------------------------

-- opt(t) : Maybe t
opt(t)
  : {- empty -}  { Nothing }
  | t            { Just $1 }


{
parseModule :: Parser ParsedModule
parseModule = parseModule_

parseImports :: Parser [Located (Import ModuleName)]
parseImports = parseImports_

parseDecls :: Parser PModule
parseDecls = parseDecls_ >>= runModuleBuilder

parseType :: Parser PType
parseType = skippingNLs parseType_

parseKind :: Parser K.Kind
parseKind = skippingNLs (fmap unL parseKind_)

parseExpr :: Parser PExp
parseExpr =  skippingNLs parseExpr_
}
