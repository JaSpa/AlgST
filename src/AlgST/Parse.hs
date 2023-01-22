{-# LANGUAGE BangPatterns #-}

module AlgST.Parse where

import AlgST.Parse.Lexer
import AlgST.Parse.ParseUtils
import AlgST.Parse.Token
import Control.Monad.Validate
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

scanTokens :: ByteString -> ParseM [Token]
scanTokens = go emptyTokenList
  where
    go :: TokenList -> AlexInput -> ParseM [Token]
    go tl !input = case alexScan input 0 of
      AlexEOF ->
        -- Forget any pending newline tokens.
        pure (runTokenList tl)
      AlexError _ ->
        -- This branch can only happen with invalid UTF8. Any unexpected
        -- characters are caught by the lexer.
        invalidUTF8 input >>= go tl
      AlexSkip input' _ ->
        -- Just continue with the remaining part.
        go tl input'
      AlexToken input' len act -> do
        -- Extract the matched part.
        let s = BS.take len input
        -- Run the action.
        case act s of
          -- In case of a diagnostic, add it to our list of diagnostics.
          Left d -> dispute (pure d) *> go tl input'
          -- If we have lexed a token, remember it and continue on.
          Right t -> go (tl `snocToken` t) input'

-- runParser :: Parser a -> ByteString -> Either Errors a
-- runParser (Parser p) src =
--   let (lexDiags, tokens) = scanTokens src
--    in runParser
