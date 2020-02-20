{-|
 - Module      : Language.SMT2.Parser
 - Description : SMT language parser
 - Maintainer  : yokis1997@pku.edu.cn
 - Stability   : experimental
 -
-}
module Language.SMT2.Parser where

import           Data.Char              (toLower)
import           Text.Parsec            (ParseError, Parsec, eof, parse, try)
import           Text.Parsec.Char
import           Text.Parsec.Combinator (between, many1, sepBy, skipMany1)
import           Text.Parsec.Prim       (many, (<?>), (<|>))
import           Text.Parsec.String     (Parser)

parseString :: Parser a -> String -> Either ParseError a
parseString p = parse p ""

parseStringEof :: Parser a -> String -> Either ParseError a
parseStringEof p = parse (p <* eof) ""

-- | Lexicons (Sec. 3.1)
--
-- Parsers for lexicons.
-- For a numeral, a decimal, or a string literal, the parsed result is the same.
-- For a hexadecimal or a binary, the result is stripped with marks (#x and #b).
--
-- Note: semantics should be provided by specific theories.
-- See Remark 1 of the refrence.
type Numeral       = String
type Decimal       = String
type Hexadecimal   = String
type Binary        = String
type StringLiteral = String
type ReservedWord  = String
type Symbol        = String
type Keyword       = String

type GenStrParser st = Parsec String st

nonZeroDigit :: GenStrParser st Char
nonZeroDigit = oneOf "123456789"

numeral :: GenStrParser st Numeral
numeral =  string "0"
       <|> do c <- nonZeroDigit
              cs <- many digit
              return (c:cs)

decimal :: GenStrParser st Decimal
decimal = do whole <- numeral
             char '.'
             zeros <- many (char '0')
             restFractional <- numeral
             return (whole <> "." <> zeros <> restFractional)

hexadecimal :: GenStrParser st Hexadecimal
hexadecimal = string "#x" >> fmap (fmap toLower) (many1 hexDigit)

binary :: GenStrParser st Binary
binary = string "#b" >> many1 (char '0' <|> char '1')


stringLiteral :: GenStrParser st StringLiteral
stringLiteral = do char '"'
                   str <- many (nonEscaped <|> escaped)
                   char '"'
                   return str
  where
    nonEscaped = noneOf "\\\""
    escaped = char '\\' >> (char '\\' <|> char '"')

reservedWord :: GenStrParser st ReservedWord
reservedWord =  string "par"
            <|> string "NUMERAL"
            <|> string "DECIMAL"
            <|> string "STRING"
            <|> string "_"
            <|> string "!"
            <|> string "as"
            <|> string "let"
            <|> string "forall"
            <|> string "exists"
            <?> "reserved words"

-- | characters allowed in a name
nameChar :: GenStrParser st Char
nameChar = oneOf  "~!@$%^&*_-+=<>.?/"

-- | a symbol should not be a reservedWord
-- so try reservedWord first
simpleSymbol :: GenStrParser st Symbol
simpleSymbol = do c <- noneOf "0123456789"
                  cs <- many (alphaNum <|> nameChar)
                  return (c:cs)

quotedSymbol :: GenStrParser st Symbol
quotedSymbol = between (char '|') (char '|') $ many (noneOf "\\|")

-- |  enclosing a simple symbol in vertical bars does not produce a
-- new symbol, e.g. abc and |abc| are the *same* symbol
-- this is guaranteed by removing the bars
symbol :: GenStrParser st Symbol
symbol = quotedSymbol <|> simpleSymbol

keyword :: GenStrParser st Keyword
keyword = do char ':'
             many1 (alphaNum <|> nameChar)

-- | S-expressions (Sec. 3.2)

data SpecConstant = SCNumeral Numeral
                  | SCDecimal Decimal
                  | SCHexadecimal Hexadecimal
                  | SCBinary Binary
                  | SCString StringLiteral

data SExpr = SEConstant SpecConstant
           | SESymbol Symbol
           | SEKeyword Keyword
           | SEList SList

type SList = [SExpr]

spaces1 :: GenStrParser st ()
spaces1 = skipMany1 space

slist :: GenStrParser st SList
slist = between (char '(') (char ')') $ sepBy sexpr spaces1

specConstant :: GenStrParser st SpecConstant
specConstant =  SCNumeral <$> numeral
            <|> SCDecimal <$> decimal
            <|> SCHexadecimal <$> hexadecimal
            <|> SCBinary <$> binary
            <|> SCString <$> stringLiteral
            <?> "spec constants"

sexpr :: GenStrParser st SExpr
sexpr =  SEConstant <$> specConstant
     <|> SESymbol <$> symbol
