{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}

module AlgST.Parse.Token where

import AlgST.Syntax.Type (Polarity)
import AlgST.Util.ErrorMessage
import AlgST.Util.Output
import AlgST.Util.SourceLocation
import Control.Applicative
import Data.DList qualified as DL
import GHC.Generics (Generic (..))

data Token
{- ORMOLU_DISABLE -}
  = TokenNL       SrcRange
  | TokenUnit     SrcRange
  | TokenLambda   SrcRange
  | TokenUnArrow  SrcRange
  | TokenLinArrow SrcRange
  | TokenLParen   SrcRange
  | TokenRParen   SrcRange
  | TokenLBracket SrcRange
  | TokenRBracket SrcRange
  | TokenComma    SrcRange
  | TokenColon    SrcRange
  | TokenPairCon  SrcRange
  | TokenMOut     SrcRange
  | TokenMIn      SrcRange
  | TokenLBrace   SrcRange
  | TokenRBrace   SrcRange
  | TokenDot      SrcRange
  | TokenUpperId  (Located String)
  | TokenLowerId  (Located String)
  | TokenOperator (Located String)
  | TokenInt      (Located Integer)
  | TokenChar     (Located Char)
  | TokenString   (Located String)
  | TokenBool     (Located Bool)
  | TokenRec      SrcRange
  | TokenLet      SrcRange
  | TokenIn       SrcRange
  | TokenEq       SrcRange
  | TokenData     SrcRange
  | TokenProtocol SrcRange
  | TokenType     SrcRange
  | TokenPipe     SrcRange
  | TokenIf       SrcRange
  | TokenThen     SrcRange
  | TokenElse     SrcRange
  | TokenNew      SrcRange
  | TokenSelect   SrcRange
  | TokenFork     SrcRange
  | TokenFork_    SrcRange
  | TokenCase     SrcRange
  | TokenOf       SrcRange
  | TokenForall   SrcRange
  | TokenDualof   SrcRange
  | TokenEnd      (Located Polarity)
  | TokenWild     SrcRange
  | TokenImport   SrcRange
  | TokenLPragma  SrcRange
  | TokenRPragma  SrcRange
  deriving stock (Show, Generic)
  deriving (HasRange) via Generically Token
{- ORMOLU_ENABLE -}

{- ORMOLU_DISABLE -}
prettyToken :: Token -> String
prettyToken = \case
  TokenNL       _ -> "\\n"
  TokenUnit     _ -> "()"
  TokenLambda   _ -> "\\"
  TokenUnArrow  _ -> "->"
  TokenLinArrow _ -> "-o"
  TokenLParen   _ -> "("
  TokenRParen   _ -> ")"
  TokenLBracket _ -> "["
  TokenRBracket _ -> "]"
  TokenComma    _ -> ","
  TokenColon    _ -> ":"
  TokenPairCon  _ -> "(,)"
  TokenMOut     _ -> "!"
  TokenMIn      _ -> "?"
  TokenLBrace   _ -> "{"
  TokenRBrace   _ -> "}"
  TokenDot      _ -> "."
  TokenRec      _ -> "rec"
  TokenLet      _ -> "let"
  TokenIn       _ -> "in"
  TokenEq       _ -> "="
  TokenData     _ -> "data"
  TokenProtocol _ -> "protocol"
  TokenType     _ -> "type"
  TokenPipe     _ -> "|"
  TokenIf       _ -> "if"
  TokenThen     _ -> "then"
  TokenElse     _ -> "else"
  TokenNew      _ -> "new"
  TokenSelect   _ -> "select"
  TokenFork     _ -> "fork"
  TokenFork_    _ -> "fork_"
  TokenCase     _ -> "case"
  TokenOf       _ -> "of"
  TokenForall   _ -> "forall"
  TokenDualof   _ -> "dual"
  TokenWild     _ -> "_"
  TokenImport   _ -> "import"
  TokenLPragma  _ -> "{-#"
  TokenRPragma  _ -> "#-}"
  TokenUpperId  (_ :@ s) -> s
  TokenLowerId  (_ :@ s) -> s
  TokenOperator (_ :@ s) -> s
  TokenInt      (_ :@ i) -> show i
  TokenChar     (_ :@ c) -> show c
  TokenString   (_ :@ s) -> show s
  TokenBool     (_ :@ b) -> show b
  TokenEnd      (_ :@ p) -> "End" ++ show p
{- ORMOLU_ENABLE -}

dropNewlines :: [Token] -> [Token]
dropNewlines = filter \case
  TokenNL _ -> False
  _ -> True

instance ErrorMsg Token where
  msg = prettyToken
  msgStyling _ = redFGStyling

data TokenList = TokenList
  { tlToks :: DL.DList Token,
    tlNL :: Maybe Token
  }

emptyTokenList :: TokenList
emptyTokenList = TokenList DL.empty Nothing

runTokenList :: TokenList -> [Token]
runTokenList = DL.toList . tlToks

snocToken :: TokenList -> Token -> TokenList
snocToken tl t@(TokenNL _) = case tlToks tl of
  -- Ignore any initial newline tokens.
  DL.Nil -> emptyTokenList
  -- Remeber the first pending newline position.
  _ -> tl {tlNL = tlNL tl <|> Just t}
snocToken tl t =
  TokenList
    { -- Insert the pending newline token before the new non-newline token.
      tlToks = tlToks tl `DL.append` foldMap DL.singleton (tlNL tl) `DL.snoc` t,
      tlNL = Nothing
    }
