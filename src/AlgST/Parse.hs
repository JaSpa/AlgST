{-# LANGUAGE BangPatterns #-}

module AlgST.Parse where

import AlgST.Parse.Lexer
import AlgST.Parse.Token
import AlgST.Util.Diagnose (Diagnostic)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

scanTokens :: ByteString -> ([Diagnostic], [Token])
scanTokens = go [] emptyTokenList
  where
    go diags tl !input = case alexScan input 0 of
      AlexEOF ->
        -- Forget any pending newline tokens.
        (diags, runTokenList tl)
      AlexError _ ->
        -- This branch can only happen with invalid UTF8. Any unexpected
        -- characters are caught by the lexer.
        --
        -- TODO: Skip over the invalid bytes and try to continue.
        (invalidUTF8 input : diags, runTokenList tl)
      AlexSkip input' _ ->
        -- Just continue with the remaining part.
        go diags tl input'
      AlexToken input' len act -> do
        -- Extract the matched part.
        let s = BS.take len input
        -- Run the action.
        case act s of
          -- In case of a diagnostic, add it to our list of diagnostics.
          Left d -> go (d : diags) tl input'
          -- If we have lexed a token, remember it and continue on.
          Right t -> go diags (tl `snocToken` t) input'
