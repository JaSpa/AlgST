{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module AlgST.Util.Diagnose
  ( -- * Types
    Diagnostic,
    Errors,
    DErrors,

    -- ** Creating Diagnostics
    err,
    warn,

    -- ** Modifiers

    -- | #modifiers#
    primary,
    context,
    fix,
    showRange,
    note,
    hint,

    -- * Rendering
    BaseDiagnostic,

    -- ** Creating the @diagnose@ representation
    buildSorted,
    addSorted,
    onlyFiles,
    addBuffer,

    -- ** Rendering the @diagnose@ values
    layoutDiagnostic,
    E.WithUnicode (..),
    P.PageWidth (..),

    -- * @MonadValidate@ integration
    MonadErrors,
    addError,
    fatalError,
    failNothing,
    runErrors,
    runErrorsT,

    -- * Writing error messages
    Doc (..),
    (<+>),
    string,
    literal,
    syntax,
    showSyntax,
    tag,
    line,
    indent,
    indent',
    unwords,
    unlines,
    joinOr,
    joinAnd,
  )
where

import AlgST.Util.SourceManager
import Control.Applicative
import Control.Category ((>>>))
import Control.Foldl qualified as L
import Control.Monad.Eta
import Control.Monad.Validate
import Data.Bifunctor
import Data.Coerce
import Data.DList.DNonEmpty (DNonEmpty, toNonEmpty)
import Data.Foldable
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty (..))
import Data.String (IsString (..))
import Error.Diagnose qualified as E
import Prettyprinter qualified as P
import Prettyprinter.Render.Terminal qualified as P
import Prelude hiding (unlines, unwords)

-- | A non-empty list of 'Diagnostic's. May contain errors and warnings,
-- despite its name.
type Errors = NonEmpty Diagnostic

-- | Difference-version of 'Errors'. Prefer this when accumulating
-- 'Diagnostic's in a monoidal context.
type DErrors = DNonEmpty Diagnostic

-- | 'Diagnostic's are combined into a @"Error.Diagnose".'E.Diagnostic'
-- String@, aka 'BaseDiagnostic', using 'addSorted' or 'buildSorted'.
--
-- 'BaseDiagnostic's can then be rendered using the functions from
-- "Error.Diagnose".
type BaseDiagnostic = E.Diagnostic EDoc

type MonadErrors = MonadValidate DErrors

addError :: (MonadErrors m) => Diagnostic -> m ()
addError !d = dispute (pure d)

fatalError :: (MonadErrors m) => Diagnostic -> m a
fatalError !d = refute (pure d)

failNothing :: (MonadErrors m) => Diagnostic -> Maybe a -> m a
failNothing d = maybe (fatalError d) pure

runErrors :: Validate DErrors a -> Either Errors a
runErrors = first toNonEmpty . runValidate

runErrorsT :: (Functor m) => ValidateT DErrors m a -> m (Either Errors a)
runErrorsT = fmap (first toNonEmpty) . runValidateT

-- | A @Diagnostic@ is either an error or a warning, including notes, hints,
-- context, etc.
--
-- A new @Diagnostic@ is created using either 'err' or 'warn'. Additional
-- information can be attached using the [modifiers](#modifiers).
newtype Diagnostic = Diagnostic (DiagInfo -> E.Report EDoc)

data DiagInfo = DiagInfo
  { diManager :: SourceManager,
    diMarkers :: [(E.Position, E.Marker Doc)],
    diNotes :: [E.Note Doc]
  }

initialDiagInfo :: SourceManager -> DiagInfo
initialDiagInfo mgr =
  DiagInfo
    { diManager = mgr,
      diMarkers = [],
      diNotes = []
    }

pushNote :: E.Note Doc -> Diagnostic -> Diagnostic
pushNote note (Diagnostic b) = Diagnostic $ oneShot \ !di -> do
  b di {diNotes = note : diNotes di}

pushMarker :: SrcRange -> E.Marker Doc -> Diagnostic -> Diagnostic
pushMarker range marker (Diagnostic b) = Diagnostic $ oneShot \ !di -> do
  let !pos = rangeToPosition di range
  b di {diMarkers = (pos, marker) : diMarkers di}

rangeToPosition :: DiagInfo -> SrcRange -> E.Position
rangeToPosition = diagnoseSrcRange . diManager

-- | Turns a list of 'Diagnostic's into a 'BaseDiagnostic'.
--
-- All files known to the given 'SourceManager' are automatically added. If you
-- need to build\/render multiple diagnostics with the same 'SourceManager' it
-- might be preferable to cache the result from 'onlyFiles' and use
-- 'addSorted'.
--
-- The given diagnostics are sorted based on their main 'SrcRange'.
buildSorted :: SourceManager -> [Diagnostic] -> BaseDiagnostic
buildSorted mgr diags = addSorted mgr diags (onlyFiles mgr)

-- | Adds a list of diagnostics to a 'BaseDiagnostic'.
--
-- The files referenced by the diagnostics have to be added in an additional
-- step, or preferably already exist in the given diagnostic.
--
-- The diagnostics are added in the order of their main 'SrcRange's. Note that
-- Diagnostics are only sorted with respect to each other, not the set of
-- diagnostics already added. Calling this function twice will always add the
-- second set of diagnostics after the first set.
addSorted ::
  SourceManager -> [Diagnostic] -> BaseDiagnostic -> BaseDiagnostic
addSorted mgr diags d0 = foldl' E.addReport d0 sortedReports
  where
    -- Builds the reports, diagnose's own representation, from each Diagnostic,
    -- our representation.
    reports = [b (initialDiagInfo mgr) | Diagnostic b <- diags]

    -- We want to display all reports in a specific order: A before B if A's
    -- primary marker is before B's primary maker.
    sortedReports = do
      List.sortOn reportPosition reports

    -- Extracts a reports markers.
    reportMarkers (E.Err _ _ ms _) = ms
    reportMarkers (E.Warn _ _ ms _) = ms

    -- Extracts a report's primary position: the earliest position of any
    -- 'This' marker.
    reportPosition :: E.Report a -> Maybe E.Position
    reportPosition = L.fold primaryPosition . reportMarkers
      where
        primaryPosition =
          (<|>)
            <$> L.prefilter isPrimary (L.premap fst L.minimum)
            <*> L.premap fst L.head

        isPrimary (_, E.This _) = True
        isPrimary _ = False

-- | Creates a 'BaseDiagnostic' without any 'E.Report's but only the set of all
-- files added to be referenced in reports.
onlyFiles :: SourceManager -> E.Diagnostic msg
onlyFiles = foldl' (flip addBuffer) mempty . managedBuffers

-- | Adds a 'Buffer' to a 'BaseDiagnostic' as a file to be referenced in a
-- 'E.Report'.
addBuffer :: Buffer -> E.Diagnostic msg -> E.Diagnostic msg
addBuffer buf diag = E.addFile diag (bufferName buf) (decodeBuffer buf)

-- | Creates an error 'Diagnostic'.
err :: String -> Diagnostic
err heading = Diagnostic $ oneShot \ !di -> do
  E.Err Nothing (getDoc (literal heading)) (coerce (diMarkers di)) (coerce (diNotes di))

-- | Creates a warning 'Diagnostic'.
warn :: String -> Diagnostic
warn heading = Diagnostic $ oneShot \ !di -> do
  E.Warn Nothing (getDoc (literal heading)) (coerce (diMarkers di)) (coerce (diNotes di))

-- | Adds a hint to a 'Diagnostic'.
hint :: Doc -> Diagnostic -> Diagnostic
hint = pushNote . E.Hint

-- | Describes the primary source of the diagnostic.
primary :: SrcRange -> Doc -> Diagnostic -> Diagnostic
primary r = pushMarker r . E.This

-- | Adds a note to a 'Diagnostic'.
note :: Doc -> Diagnostic -> Diagnostic
note = pushNote . E.Note

-- | Adds some additional context to a 'Diagnostic'.
context :: SrcRange -> Doc -> Diagnostic -> Diagnostic
context r = pushMarker r . E.Where

-- | Adds a potential fix to a 'Diagnostic'.
--
-- For longer messages prefer 'hint'.
fix :: SrcRange -> Doc -> Diagnostic -> Diagnostic
fix r = pushMarker r . E.Maybe

-- | Include some additional range in the output. No highlighting is applied.
showRange :: SrcRange -> Diagnostic -> Diagnostic
showRange r = pushMarker r E.Blank

layoutDiagnostic :: E.WithUnicode -> P.PageWidth -> BaseDiagnostic -> P.SimpleDocStream P.AnsiStyle
layoutDiagnostic unicode width =
  E.prettyDiagnostic' unicode (E.TabSize 2)
    >>> P.layoutPretty (P.LayoutOptions width)
    >>> P.reAnnotateS style

data Ann
  = -- | The annotated part is a piece of syntax
    AnnSyn
  | -- | The annotated part is a tag that is not as important as the
    -- sourroundings.
    AnnTag

-- TODO: Decide on a styling for annotations.
style :: E.Style Ann
style =
  E.defaultStyle . fmap \case
    AnnSyn -> P.colorDull P.Green <> P.bold
    AnnTag -> mempty

type EDoc = P.Doc Ann

newtype Doc = Doc {getDoc :: EDoc}
  deriving newtype (Monoid, Semigroup, Show)

instance IsString Doc where
  fromString = string

infixr 6 <+>

(<+>) :: Doc -> Doc -> Doc
Doc x <+> Doc y = Doc $ x <> P.softline <> y

string :: String -> Doc
string = Doc . P.fillSep . fmap P.pretty . words

literal :: (P.Pretty a) => a -> Doc
literal = Doc . P.pretty

tag :: Doc -> Doc
tag = coerce $ P.annotate AnnTag

showSyntax :: (Show a) => a -> Doc
showSyntax = syntax . show

syntax :: String -> Doc
syntax = Doc . P.annotate AnnSyn . P.pretty

line :: Doc
line = Doc P.hardline

indent :: Doc -> Doc
indent = indent' 2

indent' :: Int -> Doc -> Doc
indent' = coerce $ P.nest @Ann

unwords :: (Foldable f) => f Doc -> Doc
unwords f
  | null f = mempty
  | otherwise = foldr1 (<+>) f

unlines :: (Foldable f) => f Doc -> Doc
unlines f
  | null f = mempty
  | otherwise = foldr1 (\x y -> x <> line <> y) f

joinOr :: NonEmpty Doc -> Doc
joinOr = joinConnector (literal "or")

joinAnd :: NonEmpty Doc -> Doc
joinAnd = joinConnector (literal "and")

joinConnector :: Doc -> NonEmpty Doc -> Doc
joinConnector _ (x :| []) = x
joinConnector c (x :| [y]) = x <> Doc P.comma <+> c <+> y
joinConnector c (x :| y : ys) = x <> Doc P.comma <+> joinConnector c (y :| ys)
