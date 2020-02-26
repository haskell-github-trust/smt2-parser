{-|
 - Module      : Language.SMT2.Parser
 - Description : SMT language parser
 - Maintainer  : yokis1997@pku.edu.cn
 - Stability   : experimental
-}
module Language.SMT2.Parser where

import           Data.Char              (toLower)
import qualified Data.List.NonEmpty     as NE
import           Text.Parsec            (ParseError, Parsec, eof, parse, try)
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Prim       (many, (<?>), (<|>))
import           Text.Parsec.String     (Parser)

parseString :: Parser a -> String -> Either ParseError a
parseString p = parse p ""

parseStringEof :: Parser a -> String -> Either ParseError a
parseStringEof p = parse (p <* eof) ""

-- * Lexicons (Sec. 3.1)
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

reservedWords :: [String]
reservedWords = [ -- General
                  "!", "," , "as", "DECIMAL", "exists", "forall", "let", "NUMERAL", "par", "STRING",
                  -- Command names
                   "assert", "check-sat", "declare-sort", "declare-fun", "define-sort",
                   "define-fun", "exit", "get-assertions", "get-assignment", "get-info", "get-option",
                   "get-proof", "get-unsat-core", "get-value", "pop", "push", "set-logic", "set-info",
                   "set-option"
                ]

-- | accept all reserved words,
-- the exact content should be checked later in the parsing procedure
reservedWord :: GenStrParser st ReservedWord
reservedWord = choice (try . string <$> reservedWords)

-- | characters allowed in a name
nameChar :: GenStrParser st Char
nameChar = oneOf  "~!@$%^&*_-+=<>.?/"

-- | a symbol should not be a reserved word
simpleSymbol :: GenStrParser st Symbol
simpleSymbol = do c <- nameChar <|> letter
                  cs <- many (alphaNum <|> nameChar)
                  return (c:cs)

quotedSymbol :: GenStrParser st Symbol
quotedSymbol = between (char '|') (char '|') $ many (noneOf "\\|")

-- |  enclosing a simple symbol in vertical bars does not produce a
-- new symbol, e.g. @abc@ and @|abc|@ are the /same/ symbol
-- this is guaranteed by removing the bars
symbol :: GenStrParser st Symbol
symbol =  quotedSymbol
      <|> simpleSymbol
      <?> "symbol"

keyword :: GenStrParser st Keyword
keyword = do char ':'
             many1 (alphaNum <|> nameChar)


-- * S-expressions (Sec. 3.2)

data SpecConstant = SCNumeral Numeral
                  | SCDecimal Decimal
                  | SCHexadecimal Hexadecimal
                  | SCBinary Binary
                  | SCString StringLiteral
  deriving (Eq, Show)

data SExpr = SEConstant SpecConstant
           | SEReservedWord ReservedWord
           | SESymbol Symbol
           | SEKeyword Keyword
           | SEList SList
  deriving (Eq, Show)

type SList = [SExpr]

-- | skip one or more spaces
spaces1 :: GenStrParser st ()
spaces1 = skipMany1 space

-- | between round brackets
betweenBrackets :: GenStrParser st a -> GenStrParser st a
betweenBrackets = between (char '(' <* spaces) (spaces *> char ')') . try

slist :: GenStrParser st SList
slist = betweenBrackets $ sepEndBy sexpr spaces1

specConstant :: GenStrParser st SpecConstant
specConstant =  SCNumeral <$> try numeral
            <|> SCDecimal <$> try decimal
            <|> SCHexadecimal <$> try hexadecimal
            <|> SCBinary <$> try binary
            <|> SCString <$> try stringLiteral
            <?> "spec constants"

sexpr :: GenStrParser st SExpr
sexpr =  SEList <$> try slist
     <|> SEConstant <$> try specConstant
     <|> SEReservedWord <$> try reservedWord
     <|> SEKeyword <$> try keyword
     <|> SESymbol <$> try symbol
     <?> "s-expressions"


-- * Identifiers (Sec 3.3)

data Identifier = IdSymbol Symbol
                | IdIndexed Symbol (NE.NonEmpty Numeral)
  deriving (Eq, Show)

idIndexed :: GenStrParser st Identifier
idIndexed = betweenBrackets $ do
  char '_'
  spaces1
  s <- symbol
  spaces1
  ns <- many1 numeral
  return $ IdIndexed s (NE.fromList ns)


identifier :: GenStrParser st Identifier
identifier =  IdSymbol <$> symbol -- ^ symbol cannot start with (, so no ambiguity
          <|> idIndexed
          <?> "identifier"

-- * Attributes (Sec. 3.4)

data AttributeValue = AttrValSpecConstant SpecConstant
                    | AttrValSymbol Symbol
                    | AttrValSList SList
  deriving (Eq, Show)

data Attribute = AttrKey Keyword
               | AttrKeyValue Keyword AttributeValue
  deriving (Eq, Show)

attributeValue :: GenStrParser st AttributeValue
attributeValue =  AttrValSpecConstant <$> specConstant
              <|> AttrValSymbol <$> symbol
              <|> AttrValSList <$> slist
              <?> "attribute value"

attribute :: GenStrParser st Attribute
attribute =  AttrKeyValue <$> try (keyword <* spaces1) <*> attributeValue
         <|> AttrKey <$> keyword

-- * Sorts (Sec 3.5)

data Sort = SortSymbol Identifier
          | SortParameter Identifier (NE.NonEmpty Sort)
  deriving (Eq, Show)

sortParameter :: GenStrParser st Sort
sortParameter = betweenBrackets $ do
  i <- identifier
  spaces1
  ss <- sepEndBy1 sort spaces1
  return $ SortParameter i (NE.fromList ss)

sort :: GenStrParser st Sort
sort =  SortSymbol <$> try identifier
    <|> sortParameter
