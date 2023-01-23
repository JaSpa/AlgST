{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module AlgST.Util.ErrorMessage
  ( ErrorMessage (..),
    ErrorMsg (..),
    Errors,
    DErrors,
    Diagnostic (..),
    DiagKind (..),
    pattern PosError,
    unlocatedError,
    MsgTag (..),
  )
where

import AlgST.Parse.Unparser
import AlgST.Syntax.Expression as E
import AlgST.Syntax.Kind qualified as K
import AlgST.Syntax.Name
import AlgST.Syntax.Pos
import AlgST.Syntax.Type qualified as T
import AlgST.Util.Output
import AlgST.Util.SourceLocation qualified as R
import Data.Coerce
import Data.DList qualified as DL
import Data.DList.DNonEmpty (DNonEmpty)
import Data.List (intercalate)
import Data.List.NonEmpty (NonEmpty)
import System.Console.ANSI

-- | Error class and instances
class ErrorMsg a where
  msg :: a -> String

  -- | The 'SGR' the output of 'msg' should use. An empty list corresponds to
  -- no special rendering.
  msgStyling :: a -> [SGR]

-- | Specifies that this part of the error message is not as important as the
-- surroundings. When rendered with colors this uses a subdued highlighting.
newtype MsgTag a = MsgTag a

data ErrorMessage where
  Error :: ErrorMsg a => a -> ErrorMessage
  ErrLine :: ErrorMessage

data DiagKind
  = DiagError

type Errors = NonEmpty Diagnostic

type DErrors = DNonEmpty Diagnostic

data Diagnostic = Diagnostic
  { diagnosticKind :: !DiagKind,
    diagnosticPos :: !Pos,
    diagnosticMsg :: DL.DList ErrorMessage
  }

pattern DLIST :: [a] -> DL.DList a
pattern DLIST xs <-
  (DL.toList -> xs)
  where
    DLIST xs = DL.fromList xs

pattern PosError :: Pos -> [ErrorMessage] -> Diagnostic
pattern PosError p errs =
  Diagnostic
    { diagnosticKind = DiagError,
      diagnosticPos = p,
      diagnosticMsg = DLIST errs
    }

{-# COMPLETE PosError #-}

unlocatedError :: [ErrorMessage] -> Diagnostic
unlocatedError = PosError ZeroPos

instance ErrorMsg DiagKind where
  msg = \case
    DiagError -> "error"
  msgStyling = \case
    DiagError -> redFGStyling

instance HasPos Diagnostic where
  pos = diagnosticPos

instance ErrorMsg a => ErrorMsg (Located a) where
  msg = msg . unL
  msgStyling = msgStyling . unL

instance ErrorMsg a => ErrorMsg (R.Located a) where
  msg = msg . R.unL
  msgStyling = msgStyling . R.unL

instance Unparse (T.XType x) => ErrorMsg (T.Type x) where
  msg = show
  msgStyling _ = redFGStyling

instance ErrorMsg String where
  msg = id
  msgStyling _ = []

instance (Unparse (E.XExp x), Unparse (T.XType x)) => ErrorMsg (E.Exp x) where
  msg = show
  msgStyling _ = redFGStyling

instance ErrorMsg ModuleName where
  msg = unModuleName
  msgStyling _ = redFGStyling

instance ErrorMsg Unqualified where
  msg = getUnqualified
  msgStyling _ = redFGStyling

instance ErrorMsg (Name stage scope) where
  msg = pprName
  msgStyling _ = redFGStyling

instance (ErrorMsg a, ErrorMsg b) => ErrorMsg (Either a b) where
  msg = either msg msg
  msgStyling = either msgStyling msgStyling

instance ErrorMsg Pos where
  msg = show
  msgStyling _ = []

instance ErrorMsg K.Kind where
  msg = show
  msgStyling _ = redFGStyling

instance ErrorMsg Int where
  msg = show
  msgStyling _ = redFGStyling

instance ErrorMsg a => ErrorMsg (MsgTag a) where
  msg = coerce (msg @a)
  msgStyling (MsgTag a) = msgStyling a ++ [SetConsoleIntensity NormalIntensity]

instance Unparse (T.XType x) => ErrorMsg [T.Type x] where
  msg = showTypeList
  msgStyling _ = redFGStyling

showTypeList :: Unparse (T.XType x) => [T.Type x] -> String
showTypeList ts = "[" ++ intercalate ", " types ++ "]"
  where
    types = map show ts
