{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StrictData #-}

module AlgST.Parse.Token where

import AlgST.Syntax.Pos
import AlgST.Syntax.Type (Polarity)
import AlgST.Util.ErrorMessage
import AlgST.Util.Output
import Control.Applicative
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.DList qualified as DL
import Data.Word
import GHC.Generics (Generic (..))

data Token
{- ORMOLU_DISABLE -}
  = TokenNL       Pos
  | TokenUnit     Pos
  | TokenLambda   Pos
  | TokenUnArrow  Pos
  | TokenLinArrow Pos
  | TokenLParen   Pos
  | TokenRParen   Pos
  | TokenLBracket Pos
  | TokenRBracket Pos
  | TokenComma    Pos
  | TokenColon    Pos
  | TokenPairCon  Pos
  | TokenMOut     Pos
  | TokenMIn      Pos
  | TokenLBrace   Pos
  | TokenRBrace   Pos
  | TokenDot      Pos
  | TokenUpperId  (Located String)
  | TokenLowerId  (Located String)
  | TokenOperator (Located String)
  | TokenInt      (Located Integer)
  | TokenChar     (Located Char)
  | TokenString   (Located String)
  | TokenBool     (Located Bool)
  | TokenRec      Pos
  | TokenLet      Pos
  | TokenIn       Pos
  | TokenEq       Pos
  | TokenData     Pos
  | TokenProtocol Pos
  | TokenType     Pos
  | TokenPipe     Pos
  | TokenIf       Pos
  | TokenThen     Pos
  | TokenElse     Pos
  | TokenNew      Pos
  | TokenSelect   Pos
  | TokenFork     Pos
  | TokenFork_    Pos
  | TokenCase     Pos
  | TokenOf       Pos
  | TokenForall   Pos
  | TokenDualof   Pos
  | TokenEnd      (Located Polarity)
  | TokenWild     Pos
  | TokenImport   Pos
  | TokenLPragma  Pos
  | TokenRPragma  Pos
  deriving stock (Show, Generic)
  -- TODO: Drop this instance in favor of working with SrcLoc/SrcRange
  deriving (HasPos) via Generically Token
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

-- * Lexer support

type AlexInput = ByteString

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte = BS.uncons

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = error "left context not implemented"
{-# WARNING alexInputPrevChar "left context not implemented" #-}

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
