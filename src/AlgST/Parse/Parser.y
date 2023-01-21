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
  , parseModule
  , parseDecls
  , parseImports
  , parseType
  , parseKind
  , parseExpr

    -- * Running Parsers
  , feedParser
  , runParser
  , runParserIO
  , runParserSimple

    -- * Re-exports
  , ParsedModule(..)
  , emptyParsedModule
  , resolveImports
  , partitionImports
  , module AlgST.Syntax.Pos
  ) where

import           Control.Category              ((>>>), (<<<))
import           Control.Monad
import           Control.Monad.State
import           Control.Monad.Trans.Maybe
import           Data.Bifunctor
import qualified Data.DList                    as DL
import qualified Data.DList.DNonEmpty          (DNonEmpty)
import qualified Data.DList.DNonEmpty          as DNE
import           Data.Foldable
import           Data.Functor
import           Data.Functor.Identity
import qualified Data.List                     as List
import           Data.List.NonEmpty            (NonEmpty(..))
import qualified Data.Map.Strict               as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Sequence                 (Seq(..))
import qualified Data.Sequence                 as Seq
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
import           AlgST.Syntax.Pos              (Pos)
import qualified AlgST.Syntax.Pos              as Pos
import qualified AlgST.Syntax.Type             as T
import           AlgST.Util
import           AlgST.Util.SourceLocation
import           AlgST.Util.Error
import           AlgST.Util.ErrorMessage
}

%name parseModule_  Module
%name parseImports_ Imports
%name parseDecls_   Decls
%name parseKind_    Kind
%name parseType_    Type
%name parseExpr_    Exp

%tokentype { Token }
%error { parseError }
%monad { ParseM } { (>>=) } { return }

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
  | Imports               {  ParsedModule (fmap mapLoc $1) emptyModule }
  | Imports nl TopItems   {% ParsedModule (fmap mapLoc $1) `fmap` runModuleBuilder $3 }

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
      selection <- $3 $! pos $1
      pure $ $1 @- Import {
        importTarget = unL $2,
        importQualifier = emptyModuleName,
        importSelection = selection
      }
    }

ImportList :: { Pos -> ParseM ImportSelection }
  -- The optional `nl` tokens allow closing the parenthesis to appear on a new
  -- line in column 0.
  : {- empty -}                       { \p -> pure $ ImportAll p mempty mempty }
  | '(*)'                             { const $ pure $ ImportAll (pos $1) mempty mempty }
  | '()'                              { const $ pure $ ImportOnly mempty }
  | '(' opt(nl) ')'                   { const $ pure $ ImportOnly mempty }
  | '(' ImportSelection opt(nl) ')'   { $2 }

ImportSelection :: { Pos -> ParseM ImportSelection }
  : ImportItems opt(',')
    { \stmtLoc -> mergeImportOnly stmtLoc (DL.toList $1) }
  | '*' ',' ImportItems opt(',')
    { \stmtLoc -> mergeImportAll stmtLoc (pos $1) (DL.toList $3) }
  | '*' opt(',')
    { \_ -> pure $ ImportAll (pos $1) mempty mempty }

ImportItems :: { DL.DList ImportItem }
  : ImportItem                        { DL.singleton $1 }
  | ImportItems ',' ImportItem        { $1 `DL.snoc` $3 }

ImportItem :: { ImportItem }
  : ImportScope UnqualifiedVar
    { mkImportItem (mapLoc . $1) $2 ImportAsIs }
  | ImportScope UnqualifiedCon
    { mkImportItem (mapLoc . $1) $2 ImportAsIs }
  | ImportScope UnqualifiedVar as '_'
    { mkImportItem (mapLoc . $1) $2 ImportHide }
  | ImportScope UnqualifiedCon as '_'
    { mkImportItem (mapLoc . $1) $2 ImportHide }
  | ImportScope UnqualifiedVar as UnqualifiedVar
    { mkImportItem (mapLoc . $1) $4 (ImportFrom (unL $2)) }
  | ImportScope UnqualifiedCon as UnqualifiedCon
    { mkImportItem (mapLoc . $1) $4 (ImportFrom (unL $2)) }

ImportScope :: { SrcRange -> Located Scope }
  : {- empty -}   { \p -> p  @- Values }
  | type          { \_ -> $1 @- Types }

-------------------------------------------------------------------------------
-- Pragmas
-------------------------------------------------------------------------------

PragmaBenchmark :: { () }
  : UPPER_ID {% do
      when (unL $1 /= "BENCHMARK") do
        let msg = "Unknown pragma ‘" ++ unL $1 ++ "’, expected ‘BENCHMARK’"
        addError (pos $1) [Error msg]
    }

Pragma :: { ModuleBuilder }
  : '{-#' PragmaBenchmark opt('!') opt(STR) TypeAtom opt(nl) TypeAtom opt(nl) '#-}' {
      insertBenchmark Benchmark
        { benchName   = foldMap unL $4
        , benchExpect = isNothing $3
        , benchT1     = $5
        , benchT2     = $7
        }
    }
  | '{-#' PragmaBenchmark opt('!') opt(STR) nl Type nl Type opt(nl) '#-}' {
      insertBenchmark Benchmark
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
      moduleValueDecl (mapLoc $1) $2
    }
  -- Function declaration
  | ProgVar ValueParams '=' Exp {
      moduleValueBinding (mapLoc $1) (fmap mapLoc $2) $4
    }
  -- Type abbreviation
  | type KindedTVar TypeParams '=' Type {% do
      let (name, mkind) = $2
      let decl = AliasDecl (pos $1) TypeAlias
            { aliasParams = fmap (first mapLoc) $3
            , aliasKind = mkind
            , aliasType = $5
            }
      pure $ moduleTypeDecl (unL name) decl
    }
  -- Datatype declaration
  | data KindedTVar TypeParams {% do
      let (name, mkind) = $2
      let decl = DataDecl (pos $1) TypeNominal
            { nominalParams = fmap (first mapLoc) $3
            , nominalKind = K.TU `fromMaybe` mkind
            , nominalConstructors = mempty
            }
      pure $ moduleTypeDecl (unL name) decl
    }
  | data KindedTVar TypeParams '=' DataCons {% do
      let (name, mkind) = $2
      let decl = DataDecl (pos $1) TypeNominal
            { nominalParams = fmap (first mapLoc) $3
            , nominalKind = K.TU `fromMaybe` mkind
            , nominalConstructors = $5
            }
      pure $ moduleTypeDecl (unL name) decl
    }
  | protocol KindedTVar TypeParams '=' DataCons {% do
      let (name, mkind) = $2
      let decl = ProtoDecl (pos $1) TypeNominal
            { nominalParams = fmap (first mapLoc) $3
            , nominalKind = K.P `fromMaybe` mkind
            , nominalConstructors = $5
            }
      pure $ moduleTypeDecl (unL name) decl
    }

TySig :: { PType }
  : ':' Type     { $2 }

TypeParams :: { [(Located (TypeVar PStage), K.Kind)] }
  : {- empty -}   { [] }
  | TypeParams1   { toList $1 }

-- A `forall` requires a non-empty list of type var bindings.
TypeParams1 :: { NonEmpty (Located (TypeVar PStage), K.Kind) }
  : bindings1(KindBind) {% $1 \(locName, _) -> Identity locName }

DataCons :: { Constructors PStage PType }
  : DataCon              {  uncurry Map.singleton $1 }
  | '|' DataCon          {  uncurry Map.singleton $2 }
  | DataCons '|' DataCon {% uncurry insertNoDuplicates $3 $1 }

DataCon :: { (ProgVar PStage, (Pos, [PType])) }
  : Constructor TypeSeq { (unL $1, (pos $1, DL.toList $2)) }

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
  : INT                            { E.Lit (pos $1) $ E.Int    (unL $1) }
  | CHAR                           { E.Lit (pos $1) $ E.Char   (unL $1) }
  | STR                            { E.Lit (pos $1) $ E.String (unL $1) }
  | '()'                           { E.Lit (pos $1) E.Unit }
  | '(,)'                          {% fatalError $ errorMisplacedPairCon @Values (pos $1) Proxy }
  | ProgVar                        { Pos.uncurryL E.Var (mapLoc $1) }
  | Constructor                    { Pos.uncurryL E.Con (mapLoc $1) }
  | '(' ExpInner ')'               {% $2 InParens }
  | '(' Exp ',' Exp ')'            { E.Pair (pos $1) $2 $4 }
  | case Exp of Cases              { E.Case (pos $1) $2 $4 }
  | new                            { E.Exp $ Left $ BuiltinNew (pos $1) }
  | fork                           { E.Exp $ Left $ BuiltinFork (pos $1) }
  | fork_                          { E.Exp $ Left $ BuiltinFork_ (pos $1) }

ETail :: { PExp }
  : LamExp                         { $1 }
  | if Exp then Exp else Exp       { E.Cond (pos $1) $2 $4 $6 }
  | let LetBind '=' Exp in Exp     { $2 (pos $1) $4 $6 }
  | RecExp                         {% $1 E.Rec }
  | let RecExp in Exp              {% $2 \p v t r ->
      E.UnLet (pos $1) v Nothing (E.Rec p v t r) $4
    }

EApp :: { PExp }
  : EAtom                          { $1 }
  | EApp EAtom                     { E.App (pos $1) $1 $2 }
  | EApp '[' TypeApps ']'          { E.foldTypeApps (const pos) $1 $3 }
  | select Constructor             { E.Select (pos $1) (mapLoc $2) }
  | select '(,)'                   { E.Select (pos $1) (mapLoc ($2 @- PairCon)) }

EAppTail :: { PExp }
  : EApp                           { $1 }
  | ETail                          { $1 }
  | EApp ETail                     { E.App (pos $1) $1 $2 }

EOps :: { Parenthesized -> ParseM PExp }
  : OpTys
    { \p -> do
        -- A single operator may be used as a function value if it is wrapped in
        -- parentheses. Without parentheses we will complain.
        when (p == TopLevel) $ void do
          -- We know that this will produce a diagnostic. We are not interested
          -- in the actual resutl.
          sectionsParenthesized TopLevel $ OperatorFirst (Just $1) (pure $1) 
        pure $1
    }
  | OpsExp
    { \ps -> sectionsParenthesized ps $1 }
  | OpTys EAppTail
    { \ps -> sectionsParenthesized ps $ OperatorFirst Nothing ($1 :| [$2]) }
  | OpTys OpsExp
    { \ps -> sectionsParenthesized ps $ $1 `opSeqCons` $2 }

ExpInner :: { Parenthesized -> ParseM PExp }
  : EOps                           { $1 }
  | EAppTail                       { const (pure $1) }

Exp :: { PExp }
  : ExpInner                       {% $1 TopLevel }

TypeApps :: { DL.DList PType }
  : Type                           { DL.singleton $1 }
  | TypeApps ',' Type              { $1 `DL.snoc` $3 }

RecExp :: { forall a. (Pos -> ProgVar PStage -> PType -> E.RecLam Parse -> a) -> ParseM a }
  : rec ProgVar TySig '=' Exp {
      \f -> case $5 of
        E.RecAbs r -> pure $ f (pos $1) (unL $2) $3 r
        _ -> fatalError $ errorRecNoTermLambda (pos $5)
    }

LetBind :: { Pos -> PExp -> PExp -> PExp }
  : ProgVarWild opt(TySig)        { \p -> E.UnLet p (unL $1) $2 }
  | Pattern                       { \p -> uncurry (E.PatLet p) (bimap mapLoc (fmap mapLoc) $1) }

LamExp :: { PExp }
  : lambda Abs Arrow Exp {% do
      let (build, Any anyTermAbs) = $2
      let (arrPos, arrMul) = $3
      when (arrMul == K.Lin && not anyTermAbs) do
        addErrors [errorNoTermLinLambda (pos $1) arrPos]
      pure $ appEndo (build (pos $1) arrMul) $4
    }

Abs :: { (Pos -> K.Multiplicity -> Endo PExp, Any) }
  : bindings1(Abs1) {% do
      let extract :: 
            Located (Either (ProgVar PStage, Maybe PType) (TypeVar PStage, K.Kind))
            -> Maybe (Located (Either (ProgVar PStage) (TypeVar PStage)))
          extract = \case
            p :@ Left (v, _)  | not (isWild v) -> Just (p @- Left v)
            p :@ Right (v, _) | not (isWild v) -> Just (p @- Right v)
            _ -> Nothing
      binds <- $1 extract
--    binds <- $1 $ \case
--          p :@ Left (v, _)  | not (isWild v) -> Just (p @- Left v)
--          p :@ Right (v, _) | not (isWild v) -> Just (p @- Right v)
--          _ -> Nothing
      let termAbs argLoc (v, t) =
            ( \lamLoc m -> Endo $ E.Abs @Parse lamLoc . E.Bind argLoc m v t
            , Any True
            )
      let typeAbs argLoc (v, k) =
            ( \lamLoc _ -> Endo $ E.TypeAbs @Parse lamLoc . K.Bind argLoc v k
            , Any False
            )
      let build loc abs =
            either (termAbs loc) (typeAbs loc) abs
      pure $ foldMap (uncurryL (build . pos)) binds
    }

Abs1 :: { Located (Either (ProgVar PStage, Maybe PType) (TypeVar PStage, K.Kind)) } 
  : wildcard(ProgVar)                  { $1 @- Left (unL $1, Nothing) }
  | '(' wildcard(ProgVar) ')'          { $2 @- Left (unL $2, Nothing) }
  | '(' wildcard(ProgVar) ':' Type ')' { $2 @- Left (unL $2, Just $4) }
  | '[' wildcard(TypeVar) ':' Kind ']' { $2 @- Right (unL $2, unL $4) }

Cases :: { PCaseMap }
  : -- An empty case is not allowed. Accepting it here allows us to provide
    -- better error messages.
    '{' '}' { emptyCaseMap }
  | -- optional nl: The closing brace may be on column 0 of a new line. Usually
    -- nl separates declarations.
    '{' CaseMap opt(',') opt(nl) '}' { $2 }

CaseMap :: { PCaseMap }
  : Case             {% $1 emptyCaseMap }
  | CaseMap ',' Case {% $3 $1 }

Case :: { PCaseMap -> ParseM PCaseMap }
  : Pattern '->' Exp { \pcm -> do
      let (con, binds) = $1
      let branch = CaseBranch
            { branchPos = pos con
            , branchBinds = fmap mapLoc binds
            , branchExp = $3
            }
      cases <- insertNoDuplicates (unL con) branch (E.casesPatterns pcm)
      pure pcm{ E.casesPatterns = cases }
    }
  | ProgVarWild '->' Exp { \pcm -> do
      let wildBranch = CaseBranch
            { branchPos = pos $1
            , branchBinds = Identity (mapLoc $1)
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
        addErrors [errorDuplicateBind (unL $2) (pos $2) (pos $4)]
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
  : Op                      { Pos.uncurryL E.Var (mapLoc $1) }
  | OpTys '[' TypeApps ']'  { E.foldTypeApps (const pos) $1 $3 }

OpsExp :: { OperatorSequence Parse }
  : EApp OpTys              { OperandFirst (Just $2) ($1 :| [$2]) }
  | EApp OpTys EAppTail     { OperandFirst Nothing   ($1 :| [$2, $3]) }
  | EApp OpTys OpsExp       { $1 `opSeqCons` $2 `opSeqCons` $3 }


-------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

-- polarised(t :: PType) :: PType
polarised(t)
  :     t              { $1 }
  | '+' polarised(t)   { $2 }
  | '-' polarised(t)   { T.Negate (pos $1) $2 :: PType }

ConSequence1 :: { NameMap Values Pos }
  : Constructor                   { Map.singleton (unL $1) (pos $1) }
  | ConSequence1 ',' Constructor  { Map.insert (unL $3) (pos $3) $1 }

ConSequence :: { NameMap Values Pos }
  : {- empty -}                   { Map.empty }
  | ConSequence1                  { $1 }
  | ConSequence1 ','              { $1 }

ProtocolSubset :: { T.ProtocolSubset Written }
  : '[' opt('^') ConSequence ']' { T.ProtocolSubset {
      T.subsetComplement = isJust $2,
      T.subsetConstructors = $3
    } }

TypeAtom :: { PType }
  : '()'                          { T.Unit (pos $1) }
  | '(,)'                         {% fatalError $ errorMisplacedPairCon @Types (pos $1) Proxy }
  | '(' Type ',' TupleType ')'    { T.Pair (pos $1) $2 $4 }
  | end                           { Pos.uncurryL T.End (mapLoc $1) }
  | TypeVar                       { Pos.uncurryL T.Var (mapLoc $1) }
  | TypeName opt(ProtocolSubset)  { Pos.uncurryL T.Con (mapLoc $1) $2 }
  | '(' Type ')'                  { $2 }

Type1 :: { PType }
  : TypeAtom                      { $1 }
  | Type1 polarised(TypeAtom)     { T.App (pos $1) $1 $2 }

Type2 :: { PType }
  : polarised(Type1)              { $1 }

Type3 :: { PType }
  : Type2                         { $1 }
  | Polarity Type2 '.' Type3      { uncurry T.Session $1 $2 $4 }

Type4 :: { PType }
  : Type3                         { $1 }
  | dualof Type4                  { T.Dualof (pos $1) $2 }

Type5 :: { PType }
  : Type4                         { $1 }
  | Type4 Arrow Type5             { uncurry T.Arrow $2 $1 $3 }
  | Forall Type5                  { $1 $2 }

Type :: { PType }
  : Type5                         { $1 }

Forall :: { PType -> PType }
  : forall TypeParams1 '.' { do
      let bind (v, k) = Endo $ T.Forall @Parse (pos $1) . Pos.uncurryL K.Bind v k
      appEndo $ foldMap bind (fmap (first mapLoc) $2)
    }

TupleType :: { PType }
  : Type               { $1 }
  | Type ',' TupleType { T.Pair (pos $1) $1 $3 }

Arrow :: { (Pos, K.Multiplicity) }
  : '->' { (pos $1, K.Un) }
  | '-o' { (pos $1, K.Lin) }

Polarity :: { (Pos, T.Polarity) }
  : '!' { (pos $1, T.Out) }
  | '?' { (pos $1, T.In) }

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
        _ -> $1 @- K.TU <$ addErrors [Pos.uncurryL errorInvalidKind (mapLoc $1)]
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

KindBind :: { (Located (TypeVar PStage), K.Kind) }
  : '(' TypeVar ':' Kind ')'  { ($2, unL $4) }
  | '(' TypeVar ')'           { ($2, K.TU) }
  | TypeVar                   { ($1, K.TU) }

KindedTVar :: { (Located (TypeVar PStage), Maybe K.Kind) }
  : TypeName ':' Kind { ($1, Just (unL $3)) }
  | TypeName          { ($1, Nothing) }

-- wildcard(v) : v ~ Located s => Located (Name s)
wildcard(v)
  : v     { $1 }
  | '_'   { $1 @- Wildcard }

-- bindings(p) :
--   (Foldable f, Eq a, Hashable a, ErrorMsg a) => (p -> f (Located a)) -> ParseM [p]
--
-- Parses a sequence of `p` and ensures that the extracted `a`s are different.
bindings(p)
  : bindings_(p) { fmap DL.toList . $1 Map.empty }

-- bindings1(p) :
--   (Foldable f, Eq a, Hashable a, ErrorMsg a) => (p -> f (Located a)) -> ParseM (NonEmpty p)
--
-- Like `bindings` but for a non-empty sequence.
bindings1(p)
  : p bindings_(p) { \(extractAs :: p -> f (Located a)) -> do
      bound <- foldM
        (flip \(p Pos.:@ a) -> insertNoDuplicates a p)
        Map.empty
        (fmap mapLoc (extractAs $1))
      ps <- $2 bound extractAs
      pure $ $1 :| DL.toList ps
    }

bindings_(p)
  : {- empty -} { \_ _ ->
      pure $ DL.empty
    }
  | bindings_(p) p { \bound extractAs -> do
      bound' <- foldM
        (flip \(p Pos.:@ a) -> insertNoDuplicates a p)
        bound
        (fmap mapLoc (extractAs $2))
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
pos :: a -> Pos
pos = error "pos transition"

mapLoc :: Located a -> Pos.Located a
mapLoc = error "pos transition"

newtype Parser a = Parser ([Token] -> ParseM a)
  deriving (Functor)

parseModule :: Parser ParsedModule
parseModule = Parser parseModule_

parseImports :: Parser [Located (Import ModuleName)]
parseImports = Parser parseImports_

parseDecls :: Parser PModule
parseDecls = Parser (parseDecls_ >=> runModuleBuilder)

parseType :: Parser PType
parseType = Parser $ parseType_ . dropNewlines

parseKind :: Parser K.Kind
parseKind = Parser $ fmap unL . parseKind_ . dropNewlines

parseExpr :: Parser PExp
parseExpr = Parser $ parseExpr_ . dropNewlines

feedParser :: Parser a -> String -> ParseM a
feedParser = flip lexer

runParser :: Parser a -> String -> Either Errors a
runParser parser = runParseM . feedParser parser

-- | Runs a parser with the contents of the provided file. This function may
-- throw for all of the reasons 'readFile' may throw.
runParserIO :: Parser a -> FilePath -> IO (Either Errors a)
runParserIO parser file = runParser parser <$> readFile file

-- | Runs a parser from on the given input string, returning either the
-- rendered errors (mode 'Plain') or the successfull result.
runParserSimple :: Parser a -> String -> Either String a
runParserSimple p = first (renderErrors Plain "" . toList) . runParser p

lexer :: String -> Parser a -> ParseM a
lexer str (Parser f) = undefined -- either fatalError f $ scanTokens str

parseError :: [Token] -> ParseM a
parseError = fatalError <<< \case
  [] -> unlocatedError [Error "Unexpected end of file."]
  t:_ -> PosError (pos t) [Error "Unexpected token", Error t]
}
