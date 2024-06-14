{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

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
    E.prettyDiagnostic,
    E.printDiagnostic,
    E.WithUnicode (..),
    E.TabSize (..),

    -- * @MonadValidate@ integration
    MonadErrors,
    addError,
    fatalError,
    failNothing,
    runErrors,
    runErrorsT,
  )
where

import AlgST.Util.SourceManager
import Control.Monad.Eta
import Control.Monad.Validate
import Data.Bifunctor
import Data.DList.DNonEmpty (DNonEmpty, toNonEmpty)
import Data.Foldable
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty)
import Data.Ord
import Error.Diagnose qualified as E

-- | A non-empty list of 'Diagnostic's. May contain errors and warnings,
-- despite its name.
type Errors = NonEmpty Diagnostic

-- | Difference-version of 'Errors'. Prefer this when accumulating
-- 'Diagnostic's in a monoidal context.
type DErrors = DNonEmpty Diagnostic

-- | 'Diagnostic's are combined into a @"Error.Diagnose".'E.Diagnostic'
-- String@, aka 'BaseDiagnostic', using 'addSorted' or 'buildSorted'.
--
-- 'BaseDiagnostic's can the be rendered using the functions from
-- "Error.Diagnose".
type BaseDiagnostic = E.Diagnostic String

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
data Diagnostic = Diagnostic !SrcRange (DiagInfo -> E.Report String)

instance HasRange Diagnostic where
  getRange (Diagnostic r _) = r

data DiagInfo = DiagInfo
  { diManager :: SourceManager,
    diMarkers :: [(E.Position, E.Marker String)],
    diNotes :: [E.Note String]
  }

pushNote :: E.Note String -> Diagnostic -> Diagnostic
pushNote note (Diagnostic r build) = Diagnostic r $ oneShot \(!di) -> do
  build di {diNotes = note : diNotes di}

pushMarker :: SrcRange -> E.Marker String -> Diagnostic -> Diagnostic
pushMarker range marker (Diagnostic r build) = Diagnostic r $ oneShot \(!di) -> do
  let !pos = rangeToPosition di range
  build di {diMarkers = (pos, marker) : diMarkers di}

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
addSorted mgr diags d0 = foldr insert d0 sortedReverse
  where
    info =
      DiagInfo
        { diManager = mgr,
          diMarkers = [],
          diNotes = []
        }

    insert (Diagnostic _ f) d = E.addReport d (f info)

    -- We can easily sort in reverse which then allows us to use a right fold
    -- to add each report to the diagnostic.
    sortedReverse = do
      -- `getRange` is a constant time operation for `Diagnostic`. Therefore
      -- overall it performs better than sortOn, using the
      -- decorate-sort-undecorate pattern.
      List.sortBy (comparing (Down . getRange)) diags

-- | Creates a 'BaseDiagnostic' without any 'E.Report's but only the set of all
-- files added to be referenced in reports.
onlyFiles :: SourceManager -> E.Diagnostic msg
onlyFiles = foldl' (flip addBuffer) mempty . managedBuffers

-- | Adds a 'Buffer' to a 'BaseDiagnostic' as a file to be referenced in a
-- 'E.Report'.
addBuffer :: Buffer -> E.Diagnostic msg -> E.Diagnostic msg
addBuffer buf diag = E.addFile diag (bufferName buf) (decodeBuffer buf)

-- | Creates an error 'Diagnostic'.
err :: SrcRange -> String -> String -> Diagnostic
err range heading message = Diagnostic range $ oneShot \di -> do
  let !p = rangeToPosition di range
  E.Err Nothing heading ((p, E.This message) : diMarkers di) (diNotes di)

-- | Creates a warning 'Diagnostic'.
warn :: SrcRange -> String -> String -> Diagnostic
warn range heading message = Diagnostic range $ oneShot \di -> do
  let !p = rangeToPosition di range
  E.Err Nothing heading ((p, E.This message) : diMarkers di) (diNotes di)

-- | Adds a hint to a 'Diagnostic'.
hint :: String -> Diagnostic -> Diagnostic
hint = pushNote . E.Hint

-- | Adds a note to a 'Diagnostic'.
note :: String -> Diagnostic -> Diagnostic
note = pushNote . E.Note

-- | Adds some additional context to a 'Diagnostic'.
context :: SrcRange -> String -> Diagnostic -> Diagnostic
context r = pushMarker r . E.Where

-- | Adds a potential fix to a 'Diagnostic'.
--
-- For longer messages prefer 'hint'.
fix :: SrcRange -> String -> Diagnostic -> Diagnostic
fix r = pushMarker r . E.Maybe

-- | Include some additional range in the output. No highlighting is applied.
showRange :: SrcRange -> Diagnostic -> Diagnostic
showRange r = pushMarker r E.Blank
