{
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StrictData #-}

module AlgST.Parse.Lexer
( Token(..)
, scanTokens
, dropNewlines
) where

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

data Token =
    TokenNL Pos
  | TokenUnit Pos
  | TokenLambda Pos
  | TokenUnArrow Pos
  | TokenLinArrow Pos
  | TokenLParen Pos
  | TokenRParen Pos
  | TokenLBracket Pos
  | TokenRBracket Pos
  | TokenComma Pos
  | TokenColon Pos
  | TokenUpperId (Located String)
  | TokenPairCon Pos
  | TokenMOut Pos
  | TokenMIn Pos
  | TokenLBrace Pos
  | TokenRBrace Pos
  | TokenDot Pos
  | TokenLowerId (Located String)
  | TokenOperator (Located String)
  | TokenInt (Located Integer)
  | TokenChar (Located Char)
  | TokenString (Located String)
  | TokenBool (Located Bool)
  | TokenRec Pos
  | TokenLet Pos
  | TokenIn Pos
  | TokenEq Pos
  | TokenData Pos
  | TokenProtocol Pos
  | TokenType Pos
  | TokenPipe Pos
  | TokenIf Pos
  | TokenThen Pos
  | TokenElse Pos
  | TokenNew Pos
  | TokenSelect Pos
  | TokenFork Pos
  | TokenFork_ Pos
  | TokenCase Pos
  | TokenOf Pos
  | TokenForall Pos
  | TokenDualof Pos
  | TokenEnd (Located Polarity)
  | TokenWild Pos
  | TokenImport Pos
  | TokenLPragma Pos
  | TokenRPragma Pos

instance Show Token where
  show (TokenNL _) = "\\n"
  show (TokenUnit _) = "()"
  show (TokenLambda _) = "λ"
  show (TokenUnArrow _) = "->"
  show (TokenLinArrow _) = "-o"
  show (TokenLParen _) = "("
  show (TokenRParen _) = ")"
  show (TokenLBracket _) = "["
  show (TokenRBracket _) = "]"
  show (TokenComma _) = ","
  show (TokenColon _) = ":"
  show (TokenUpperId (_ :@ c)) = c
  show (TokenPairCon _) = "(,)"
  show (TokenMOut _) = "!"
  show (TokenMIn _) = "?"
  show (TokenLBrace _) = "{"
  show (TokenRBrace _) = "}"
  show (TokenDot _) = "."
  show (TokenLowerId (_ :@ s)) = s
  show (TokenOperator (_ :@ s)) = s
  show (TokenInt (_ :@ i)) = show i
  show (TokenChar (_ :@ c)) = show c
  show (TokenBool (_ :@ b)) = show b
  show (TokenString (_ :@ s)) = s
  show (TokenRec _) = "rec"
  show (TokenLet _) = "let"
  show (TokenIn _) = "in"
  show (TokenEq _) = "="
  show (TokenData _) = "data"
  show (TokenProtocol _) = "protocol"
  show (TokenType _) = "type"
  show (TokenPipe _) = "|"
  show (TokenIf _) = "if"
  show (TokenThen _) = "then"
  show (TokenElse _) = "else"
  show (TokenNew _) = "new"
  show (TokenSelect _) = "select"
  show (TokenFork _) = "fork"
  show (TokenFork_ _) = "fork_"
  show (TokenCase _) = "case"
  show (TokenForall _) = "forall"
  show (TokenWild _) = "_"
  show (TokenOf _) = "of"
  show (TokenDualof _) = "dualof"
  show (TokenEnd (_ :@ p)) = "End" ++ show p
  show (TokenImport _) = "import"
  show (TokenLPragma _) = "{-#"
  show (TokenRPragma _) = "#-}"

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

instance ErrorMsg Token where
  msg = show
  msgStyling _ = redFGStyling

getLineNum :: AlexPosn -> Int
getLineNum (AlexPn _offset lineNum _colNum) = lineNum

getColumnNum :: AlexPosn -> Int
getColumnNum (AlexPn _offset _lineNum colNum) = colNum

dropNewlines :: [Token] -> [Token]
dropNewlines = filter \case
  TokenNL _ -> False
  _         -> True

-- POSITIONS

internalPos :: AlexPosn -> Pos
internalPos (AlexPn _ l c) = Pos l c

instance HasPos Token where
  pos (TokenNL p) = p
  pos (TokenUnit p) = p
  pos (TokenLambda p) = p
  pos (TokenUnArrow p) = p
  pos (TokenLinArrow p) = p
  pos (TokenLParen p) = p
  pos (TokenRParen p) = p
  pos (TokenLBracket p) = p
  pos (TokenRBracket p) = p
  pos (TokenComma p) = p
  pos (TokenColon p) = p
  pos (TokenUpperId (p :@ _)) = p
  pos (TokenPairCon p) = p
  pos (TokenMOut p) = p
  pos (TokenMIn p) = p
  pos (TokenLBrace p) = p
  pos (TokenRBrace p) = p
  pos (TokenDot p) = p
  pos (TokenLowerId (p :@ _)) = p
  pos (TokenOperator (p :@ _)) = p
  pos (TokenInt (p :@ _)) = p
  pos (TokenChar (p :@ _)) = p
  pos (TokenBool (p :@ _)) = p
  pos (TokenString (p :@ _)) = p
  pos (TokenRec p) = p
  pos (TokenLet p) = p
  pos (TokenIn p) = p
  pos (TokenEq p) = p
  pos (TokenData p) = p
  pos (TokenProtocol p) = p
  pos (TokenType p) = p
  pos (TokenPipe p) = p
  pos (TokenNew p) = p
  pos (TokenSelect p) = p
  pos (TokenFork p) = p
  pos (TokenFork_ p) = p
  pos (TokenCase p) = p
  pos (TokenForall p) = p
  pos (TokenWild p) = p
  pos (TokenIf p) = p
  pos (TokenThen p) = p
  pos (TokenElse p) = p
  pos (TokenOf p) = p
  pos (TokenDualof p) = p
  pos (TokenEnd (p :@ _)) = p
  pos (TokenImport p) = p
  pos (TokenLPragma p) = p
  pos (TokenRPragma p) = p

simpleToken :: (Pos -> t) -> AlexPosn -> a -> t
simpleToken t p _ = t (internalPos p)

textToken :: (Located String -> t) -> AlexPosn -> String -> t
textToken = textToken' id

textToken' :: (String -> a) -> (Located a -> t) -> AlexPosn -> String -> t
textToken' f t p s = t (internalPos p @- f s)
}
