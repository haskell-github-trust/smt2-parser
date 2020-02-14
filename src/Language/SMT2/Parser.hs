{-|
 - Module      : Language.SMT2.Parser
 - Description : SMT language parser
 - Maintainer  : yokis1997@pku.edu.cn
 - Stability   : experimental
 -
-}
module Language.SMT2.Parser where

import           Data.Char                          (toLower)
import           Text.Parsec                        (ParseError, eof, parse)
import           Text.Parsec.String                 (Parser)
import           Text.ParserCombinators.Parsec.Char (char, digit, hexDigit,
                                                     oneOf, satisfy, string)
import           Text.ParserCombinators.Parsec.Prim (GenParser, many, (<|>))

parseFromString :: Parser a -> String -> Either ParseError a
parseFromString p = parse p ""

parseFromStringEof :: Parser a -> String -> Either ParseError a
parseFromStringEof p = parse (p <* eof) ""

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

nonZeroDigit :: GenParser Char st Char
nonZeroDigit = oneOf "123456789"

numeral :: GenParser Char st Numeral
numeral =  string "0"
       <|> do head <- nonZeroDigit
              tail <- many digit
              return (head:tail)

decimal :: GenParser Char st Decimal
decimal = do whole <- numeral
             char '.'
             zeros <- many (char '0')
             restFractional <- numeral
             return (whole <> "." <> zeros <> restFractional)

hexadecimal :: GenParser Char st Hexadecimal
hexadecimal = string "#x" *> fmap (fmap toLower) (many hexDigit)


-- | S-expressions (Sec. 3.2)



