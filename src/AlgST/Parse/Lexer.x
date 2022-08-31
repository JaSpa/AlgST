{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

module AlgST.Parse.Lexer
( Token(..)
, scanTokens
, dropNewlines
) where

import AlgST.Parse.Token
import AlgST.Syntax.Pos
import AlgST.Syntax.Type (Polarity(..))
import AlgST.Util.ErrorMessage
import AlgST.Util.Output
import Control.Applicative
import Data.DList qualified as DL
}

%wrapper "posn"

$upper  = [A-Z]
$lower  = [a-z]
$digit  = [0-9]
$alphaNumeric = [$upper $lower $digit \_ \']

@lowerId = $lower $alphaNumeric* | "_" $alphaNumeric+
@upperId = $upper $alphaNumeric*

$opsymbol = [\!\?\#\$\%\&\+\-\*\/\<\=\>\@\\\^\|\~\:]
@operator = [$opsymbol # [\!\?]] $opsymbol*

-- Supported escape sequences:
--
--  \n \" \' \\
@escape = \\ [\\ \" \' n]
@char   = \' (@escape | [^ \\ \']) \'
@string = \" (@escape | [^ \\ \"])* \"

$eol = [\n]

-- The regex looks strange because complementing a character set never includes
-- the newline character. We can use nested character sets to create the
-- desired set.
@blockComment0 = "{--}"
@blockComment1 = "{-" [. $eol] "-}"
@blockCommentM = "{-" [[^\#] $eol] ([[^\-] $eol] | \-[[^\}] $eol])* [[^\#] $eol] "-}"

tokens :-
  "{-#"                         { simpleToken TokenLPragma }
  "#-}"                         { simpleToken TokenRPragma }
  $white*$eol+                  { simpleToken TokenNL }
  $white+                       ;
  $white*"--".*                 ;
  @blockComment0                ;
  @blockComment1                ;
  @blockCommentM                ;
  ("->"|→)                      { simpleToken TokenUnArrow }
  ("-o"|⊸)                      { simpleToken TokenLinArrow }
  [\\ λ]                        { simpleToken TokenLambda }
  "("                           { simpleToken TokenLParen }
  ")"                           { simpleToken TokenRParen }
  "["                           { simpleToken TokenLBracket }
  "]"                           { simpleToken TokenRBracket }
  "{"                           { simpleToken TokenLBrace }
  "}"                           { simpleToken TokenRBrace }
  ","                           { simpleToken TokenComma }
  ":"                           { simpleToken TokenColon }
  "!"                           { simpleToken TokenMOut }
  "?"                           { simpleToken TokenMIn }
  "."                           { simpleToken TokenDot }
  "="                           { simpleToken TokenEq }
  "|"                           { simpleToken TokenPipe }
  "_"                           { simpleToken TokenWild }
-- Keywords
  rec                           { simpleToken TokenRec }
  let                           { simpleToken TokenLet }
  in                            { simpleToken TokenIn }
  data                          { simpleToken TokenData }
  protocol                      { simpleToken TokenProtocol }
  type                          { simpleToken TokenType }
  if                            { simpleToken TokenIf }
  then                          { simpleToken TokenThen }
  else                          { simpleToken TokenElse }
  new                           { simpleToken TokenNew }
  select                        { simpleToken TokenSelect }
  fork                          { simpleToken TokenFork }
  fork_                         { simpleToken TokenFork_ }
  (case|match)                  { simpleToken TokenCase }
  (of|with)                     { simpleToken TokenOf }
  (forall|∀)                    { simpleToken TokenForall }
  dual                          { simpleToken TokenDualof }
  "End!"                        { simpleToken \p -> TokenEnd (p :@ Out) }
  "End?"                        { simpleToken \p -> TokenEnd (p :@ In)  }
  import                        { simpleToken TokenImport }
-- Values
  \(\)                          { simpleToken TokenUnit }
  (0+|[1-9]$digit*)             { textToken' read TokenInt }
  @char                         { textToken' read TokenChar }
  @string                       { textToken' read TokenString }
-- Identifiers
  @operator                     { textToken TokenOperator }
  "(" @operator ")"             { textToken TokenLowerId }
  @lowerId                      { textToken TokenLowerId }
  @upperId                      { textToken TokenUpperId }
  "(,)"                         { simpleToken TokenPairCon }

{
data TokenList = TokenList
  { tl_toks :: DL.DList Token
  , tl_nl   :: Maybe Token
  }

emptyTokenList :: TokenList
emptyTokenList = TokenList DL.empty Nothing

snocToken :: TokenList -> Token -> TokenList
snocToken tl t@(TokenNL _) = case tl_toks tl of
  -- Ignore any initial newline tokens.
  DL.Nil -> emptyTokenList
  -- Remeber the first pending newline position.
  _ -> tl { tl_nl = tl_nl tl <|> Just t }
snocToken tl t = TokenList
  -- Insert the pending newline token before the new non-newline token.
  { tl_toks = tl_toks tl `DL.append` foldMap DL.singleton (tl_nl tl) `DL.snoc` t
  , tl_nl   = Nothing
  }

scanTokens :: String -> Either Diagnostic [Token]
scanTokens str = go emptyTokenList (alexStartPos, '\n', [], str)
  where
    go !tl inp@(pos,_,_,str) =
      case alexScan inp 0 of
        AlexEOF ->
          -- Forget any pending newline tokens.
          Right $ DL.toList $ tl_toks tl
        AlexError _ ->
          Left $ PosError (internalPos pos)
            [ Error "Unexpected error on input"
            , Error $ Unexpected $ head str
            ]
        AlexSkip  inp' _len ->
          go tl inp'
        AlexToken inp' len act -> do
          let !t = act pos (take len str)
          go (tl `snocToken` t) inp'

newtype Unexpected = Unexpected Char

instance ErrorMsg Unexpected where
  msg (Unexpected c) = show c
  msgStyling _ = redFGStyling

getLineNum :: AlexPosn -> Int
getLineNum (AlexPn _offset lineNum _colNum) = lineNum

getColumnNum :: AlexPosn -> Int
getColumnNum (AlexPn _offset _lineNum colNum) = colNum

-- POSITIONS

internalPos :: AlexPosn -> Pos
internalPos (AlexPn _ l c) = Pos l c

simpleToken :: (Pos -> t) -> AlexPosn -> a -> t
simpleToken t p _ = t (internalPos p)

textToken :: (Located String -> t) -> AlexPosn -> String -> t
textToken = textToken' id

textToken' :: (String -> a) -> (Located a -> t) -> AlexPosn -> String -> t
textToken' f t p s = t (internalPos p @- f s)
}
