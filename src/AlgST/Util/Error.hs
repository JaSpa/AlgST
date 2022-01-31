{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | This module provides tools to prettify & format errors with ANSI colors
-- for terminals.
module AlgST.Util.Error
  ( Diagnostic,
    OutputMode (..),
    renderErrors,
    renderErrors',
    formatErrorMessages,
    internalError,
  )
where

import AlgST.Util
import AlgST.Util.ErrorMessage
import AlgST.Util.Output
import Data.CallStack
import Data.Foldable
import Data.Function
import qualified Data.List as List
import Data.Maybe
import Data.Monoid (Endo (..))
import Syntax.Base
import Prelude hiding (truncate)

-- | Renders a single error as 'Plain'. Multiple errors are rendered
-- interspersed with newlines and not truncated.
instance Show Diagnostic where
  showsPrec _ e = showList [e]
  showList = renderErrors' Nothing Plain ""

-- | Renders a list of errors for the given 'OutputMode'. The errors are
-- ordered by location and then truncated to at most ten.
renderErrors :: OutputMode -> String -> [Diagnostic] -> String
renderErrors m fileName errs = renderErrors' (Just 10) m fileName errs ""

-- | Like 'renderErrors' but truncates based on the first parameter.
renderErrors' :: Maybe Int -> OutputMode -> String -> [Diagnostic] -> ShowS
renderErrors' maxErrs mode fileName errs =
  sortPos errs
    & fmap (Endo . render)
    & maybe id (\n -> truncate n truncMsg) maxErrs
    & List.intersperse (Endo $ showChar '\n')
    & fold
    & appEndo
  where
    render = formatErrorMessagesS mode fileName
    truncMsg = Endo $ showString "\nToo many errors. Truncated."

-- | Formats a single error.
formatErrorMessages :: OutputMode -> String -> Diagnostic -> String
formatErrorMessages mode fname err = formatErrorMessagesS mode fname err ""

formatErrorMessagesS :: OutputMode -> String -> Diagnostic -> ShowS
formatErrorMessagesS mode fname Diagnostic {..}
  | null diagnosticMsg = id
  | otherwise = header . appEndo body
  where
    header = styleHeader mode diagnosticKind fname diagnosticPos
    body = foldMap (Endo . styledMessage mode) diagnosticMsg

styledMessage :: OutputMode -> ErrorMessage -> ShowS
styledMessage mode = \case
  Error e -> showChar ' ' . styledMessagePart mode e
  ErrLine -> linebreak

styledMessagePart :: ErrorMsg a => OutputMode -> a -> ShowS
styledMessagePart mode a =
  -- Every part has to be bolded by itself instead of the whole message because
  -- resetting the color after a colored part in the error message would reset
  -- the boldness for the rest of the message as well.
  applyStyle mode (style (boldSGR : msgStyling a)) (msg a ++)

styleHeader :: OutputMode -> DiagKind -> String -> Pos -> ShowS
styleHeader mode kind f p =
  start . location . endSpace . diagKind
  where
    start
      | null f = showChar '\n'
      | otherwise = bold $ showChar '\n' . showString f . showChar ':'
    location
      | p == defaultPos = id
      | otherwise = bold $ shows p . showChar ':'
    endSpace
      | null f && p == defaultPos = id
      | otherwise = showChar ' '
    diagKind =
      styledMessagePart mode kind . bold (showChar ':') . linebreak
    bold =
      applyStyle mode styleBold

linebreak :: ShowS
linebreak = showString "\n\t"

-- | Internal errors
internalError :: (HasCallStack, Show a, Position a) => String -> a -> b
internalError fun syntax =
  error $
    show (pos syntax)
      ++ ": Internal error at "
      ++ fun
      ++ ": "
      ++ show syntax
