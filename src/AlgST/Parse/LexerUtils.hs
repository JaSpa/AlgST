{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module AlgST.Parse.LexerUtils where

import AlgST.Parse.Token
import AlgST.Util.Diagnose qualified as D
import AlgST.Util.Operators
import AlgST.Util.SourceManager
import Control.Monad.Reader
import Control.Monad.Validate
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Function ((&))
import Data.Word
import GHC.Foreign qualified as GHC
import Numeric (showHex)
import System.IO qualified as IO
import System.IO.Unsafe

data NewlinePolicy
  = -- | Don't report any immediately following 'TokenNL' tokens. Behaves like
    -- 'NLSkipConsecutive' once a proper token has been returned.
    NLSkipFollowing
  | -- | Report the next 'TokenNL' token but skip over consecutive 'TokenNL'
    -- tokens.
    NLSkipConsecutive
  | -- | Don't report any 'TokenNL' tokens to the parser.
    NLSkipAll

-- | Changes the policy after reading a non 'TokenNL' token.
--
-- 'NLSkipAll' stays as 'NLSkipAll' wheras both 'NLSkipFollowing' and
-- 'NLSkipConsecutive' become 'NLSkipConsecutive'.
policyAfterToken :: NewlinePolicy -> NewlinePolicy
policyAfterToken NLSkipAll = NLSkipAll
policyAfterToken _ = NLSkipConsecutive

newtype LexFn = LexFn {runLexFn :: forall a. (Token -> Parser a) -> Parser a}

newtype Parser a = Parser {unParser :: ReaderT (NewlinePolicy, NewlinePolicy -> LexFn) (Validate D.DErrors) a}
  deriving newtype (Functor, Applicative, Monad)
  deriving newtype (MonadValidate D.DErrors)

type LexAction = ByteString -> Either D.Diagnostic Token

type AlexInput = ByteString

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte = BS.uncons

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = error "left context not implemented"
{-# WARNING alexInputPrevChar "left context not implemented" #-}

simpleToken :: (SrcRange -> Token) -> LexAction
simpleToken f = Right . f . fullRange

textToken :: (Located String -> Token) -> LexAction
textToken = textToken' id

textToken' :: (String -> a) -> (Located a -> Token) -> LexAction
textToken' f g s = Right $ g $ fullRange s @- f decoded
  where
    decoded = unsafeDupablePerformIO do
      BS.useAsCStringLen s (GHC.peekCStringLen IO.utf8)

invalidChar :: LexAction
invalidChar s = Left do
  D.err "invalid source character"
    & D.primary (fullRange s) "skipping this character, trying to continue"

-- | Emits an error about invalid UTF-8. We try to recover and return the
-- remaining input.
invalidUTF8 :: (MonadValidate D.DErrors m) => AlexInput -> m AlexInput
invalidUTF8 s =
  -- TODO: Implement recovery.
  refute (pure err)
  where
    err =
      D.err "invalid UTF-8"
        & D.primary
          (SizedRange (startLoc s) 1)
          ("unexpected byte" <+> D.literal ("0x" ++ showByte (BS.head s)))
        & D.note "I stopped reading the input file here."
    showByte b =
      "0x" ++ case showHex b "" of
        hex@[_] -> '0' : hex
        hex -> hex
