{
{-# LANGUAGE BangPatterns #-}

module AlgST.Parse.Lexer (AlexInput, AlexReturn(..), alexScan) where

import AlgST.Parse.ParseUtils
import AlgST.Parse.Token
import AlgST.Syntax.Type (Polarity(..))
import AlgST.Util.SourceLocation
}

%action "LexAction"

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
-- The error token. This matches a full character if no other rule matches.
  .                             { invalidChar }
