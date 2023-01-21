module AlgST.Util.Diagnose
  ( Diagnostic,
    err,
    warn,
    context,
    fix,
    showRange,
    note,
    hint,
    buildDiagnostic,
  )
where

import AlgST.Util.SourceManager
import Control.Monad.Reader
import Data.Foldable
import Data.List qualified as List
import Data.Ord
import Error.Diagnose qualified as E

data DiagInfo = DiagInfo
  { diManager :: SourceManager,
    diMarkers :: [(E.Position, E.Marker String)],
    diNotes :: [E.Note String]
  }

pushNote :: E.Note String -> Diagnostic -> Diagnostic
pushNote note = modify \di -> di {diNotes = note : diNotes di}

pushMarker :: SrcRange -> E.Marker String -> Diagnostic -> Diagnostic
pushMarker range marker diag = Diagnostic do
  pos <- rangeToPosition range
  unDiagnostic $ modify (\di -> di {diMarkers = (pos, marker) : diMarkers di}) diag

modify :: (DiagInfo -> DiagInfo) -> Diagnostic -> Diagnostic
modify f (Diagnostic d) = Diagnostic (local f d)

newtype Diagnostic = Diagnostic {unDiagnostic :: Reader DiagInfo (SrcRange, E.Report String)}

rangeToPosition :: SrcRange -> Reader DiagInfo E.Position
rangeToPosition r = asks \di -> diagnoseSrcRange (diManager di) r

-- | Turns a set of 'Diagnostic's into a @"Error.Diagnose".'E.Diagnostic'@.
--
-- All diagnostics are presorted based on their main source range.
buildDiagnostic :: Foldable f => SourceManager -> f Diagnostic -> E.Diagnostic String
buildDiagnostic mgr diags = foldr (flip E.addReport) foldedSources sorted
  where
    info =
      DiagInfo
        { diManager = mgr,
          diMarkers = [],
          diNotes = []
        }
    sorted = fmap snd do
      let reports = [runReader diag info | Diagnostic diag <- toList diags]
      List.sortBy (comparing fst) reports
    foldedSources =
      foldl'
        (\diag buf -> E.addFile diag (bufferName buf) (decodeBuffer buf))
        mempty
        (managedBuffers mgr)

err :: SrcRange -> String -> String -> Diagnostic
err range heading message = Diagnostic do
  di <- ask
  p <- rangeToPosition range
  pure (range, E.Err Nothing heading ((p, E.This message) : diMarkers di) (diNotes di))

warn :: SrcRange -> String -> String -> Diagnostic
warn range heading message = Diagnostic do
  di <- ask
  p <- rangeToPosition range
  pure (range, E.Err Nothing heading ((p, E.This message) : diMarkers di) (diNotes di))

hint :: String -> Diagnostic -> Diagnostic
hint = pushNote . E.Hint

note :: String -> Diagnostic -> Diagnostic
note = pushNote . E.Note

context :: SrcRange -> String -> Diagnostic -> Diagnostic
context r = pushMarker r . E.Where

fix :: SrcRange -> String -> Diagnostic -> Diagnostic
fix r = pushMarker r . E.Maybe

showRange :: SrcRange -> Diagnostic -> Diagnostic
showRange r = pushMarker r E.Blank
