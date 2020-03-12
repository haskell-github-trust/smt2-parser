{-|
 - Module      : Language.SMT2.Parser
 - Description : SMT language parser
 - Maintainer  : yokis1997@pku.edu.cn
 - Stability   : experimental
-}
module Language.SMT2.Parser where

import           Data.Char              (toLower)
import           Data.Functor           (($>))
import           Data.List.NonEmpty     (NonEmpty, fromList)
import           Language.SMT2.Syntax
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

type GenStrParser st = Parsec String st

numeral :: GenStrParser st Numeral
numeral =  string "0"
       <|> do c <- oneOf "123456789"
              cs <- many digit
              return (c:cs)

decimal :: GenStrParser st Decimal
decimal = do whole <- numeral
             char '.'
             zeros <- many (char '0')
             restFractional <- option "" numeral
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
    escaped = char '\\' >> (char '\\' <|> char '"' <|> pure '\\') -- try to escape, if can't, preserve the backslash

reservedWords :: [String]
reservedWords = [ -- General
                  "!", "_" , "as", "DECIMAL", "exists", "forall", "let", "NUMERAL", "par", "STRING",
                  -- Command names
                   "assert", "check-sat", "declare-sort", "declare-fun", "define-sort",
                   "define-fun", "exit", "get-assertions", "get-assignment", "get-info", "get-option",
                   "get-proof", "get-unsat-core", "get-value", "pop", "push", "set-logic", "set-info",
                   "set-option"
                ]

-- | accept all reserved words,
-- the exact content should be checked later in the parsing procedure
reservedWord :: GenStrParser st ReservedWord
reservedWord = choice (parseWord <$> reservedWords)
  where
    parseWord :: String -> GenStrParser st ReservedWord
    parseWord w = try (string w) <* notFollowedBy anyChar


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
symbol =  notFollowedBy (try reservedWord) >> (quotedSymbol <|> simpleSymbol <?> "symbol")

keyword :: GenStrParser st Keyword
keyword = do char ':'
             many1 (alphaNum <|> nameChar)


-- * S-expressions (Sec. 3.2)

-- ** Utils

-- | skip one or more spaces
spaces1 :: GenStrParser st ()
spaces1 = skipMany1 space

-- | between round brackets
betweenBrackets :: GenStrParser st a -> GenStrParser st a
betweenBrackets = between (char '(' <* spaces) (spaces *> char ')') . try

-- | many p, separated by spaces1, possibly has a trailing spaces1
sepSpace :: GenStrParser st a -> GenStrParser st [a]
sepSpace p = sepEndBy p spaces1

-- | many1 p, separated by spaces1, possibly has a trailing spaces1
sepSpace1 :: GenStrParser st a -> GenStrParser st (NonEmpty a)
sepSpace1 p = fromList <$> sepEndBy1 p spaces1

slist :: GenStrParser st SList
slist = betweenBrackets . sepSpace $ sexpr

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

identifier :: GenStrParser st Identifier
identifier =  IdSymbol <$> symbol -- ^ symbol cannot start with (, so no ambiguity
          <|> idIndexed
          <?> "identifier"
  where
    idIndexed = betweenBrackets $ do
      char '_' <* spaces1
      s <- symbol <* spaces1
      IdIndexed s <$> sepSpace1 numeral


-- * Attributes (Sec. 3.4)

attributeValue :: GenStrParser st AttributeValue
attributeValue =  AttrValSpecConstant <$> specConstant
              <|> AttrValSymbol <$> symbol
              <|> AttrValSList <$> slist
              <?> "attribute value"

attribute :: GenStrParser st Attribute
attribute =  AttrKeyValue <$> try (keyword <* spaces1) <*> attributeValue
         <|> AttrKey <$> keyword
         <?> "attribute"


-- * Sorts (Sec 3.5)

sortParameter :: GenStrParser st Sort
sortParameter = betweenBrackets $ do
  i <- identifier <* spaces1
  SortParameter i <$> sepSpace1 sort

sort :: GenStrParser st Sort
sort =  SortSymbol <$> try identifier
    <|> sortParameter
    <?> "sort"


-- * Terms and Formulas (Sec 3.6)

qualIdentifier :: GenStrParser st QualIdentifier
qualIdentifier =  Unqualified <$> try identifier
              <|> betweenBrackets annotation
              <?> "qual identifier"
  where
    annotation :: GenStrParser st QualIdentifier
    annotation = do
      string "as"
      id <- identifier <* spaces1
      Qualified id <$> sort

varBinding :: GenStrParser st VarBinding
varBinding = betweenBrackets $ VarBinding <$> symbol <* spaces1 <*> term

sortedVar :: GenStrParser st SortedVar
sortedVar = betweenBrackets $ SortedVar <$> symbol <* spaces1 <*> sort

term :: GenStrParser st Term
term =  TermSpecConstant <$> try specConstant
    <|> TermQualIdentifier <$> try qualIdentifier
    <|> try application
    <|> try binding
    <|> try quantifyForall
    <|> try quantifyExists
    <|> try annotation
    <?> "term"
  where
    application = betweenBrackets $ do
      id <- qualIdentifier <* spaces1
      TermApplication id <$> sepSpace1 term
    binding = betweenBrackets $ do
      string "let" <* spaces1
      vbs <- betweenBrackets $ sepSpace1 varBinding
      TermLet vbs <$> term
    quantifyForall = betweenBrackets $ do
      string "forall" <* spaces1
      svs <- betweenBrackets $ sepSpace1 sortedVar
      TermForall svs <$> term
    quantifyExists = betweenBrackets $ do
      string "exists" <* spaces1
      svs <- betweenBrackets $ sepSpace1 sortedVar
      TermExists svs <$> term
    annotation = betweenBrackets $ do
      char '!' <* spaces1
      t <- term <* spaces1
      TermAnnotation t <$> sepSpace1 attribute


-- * Theory declarations (Sec 3.7)

sortSymbolDecl :: GenStrParser st SortSymbolDecl
sortSymbolDecl = betweenBrackets $ do
  i <- identifier <* spaces1
  n <- numeral <* spaces1
  SortSymbolDecl i n <$> sepSpace attribute

metaSpecConstant :: GenStrParser st MetaSpecConstant
metaSpecConstant =  string "NUMERAL" $> MSC_NUMERAL
                <|> string "DECIMAL" $> MSC_DECIMAL
                <|> string "STRING"  $> MSC_STRING
                <?> "meta spec constant"

funSymbolDecl :: GenStrParser st FunSymbolDecl
funSymbolDecl =  try (betweenBrackets funConstant)
             <|> try (betweenBrackets funMeta)
             <|> try (betweenBrackets funIdentifier)
             <?> "non-parametric function symbol declaration"
  where
    funConstant = do
      sc <- specConstant <* spaces1
      s <- sort <* spaces1
      FunConstant sc s <$> sepSpace attribute
    funMeta = do
      m <- metaSpecConstant <* spaces1
      s <- sort <* spaces1
      FunMeta m s <$> sepSpace attribute
    funIdentifier = do
      i <- identifier <* spaces1
      ss <- sepSpace1 sort
      as <- sepSpace attribute
      return $ FunIdentifier i ss as

parFunSymbolDecl :: GenStrParser st ParFunSymbolDecl
parFunSymbolDecl =  NonPar <$> funSymbolDecl
                <|> betweenBrackets par
                <?> "potentially parametric function symbol declaration"
  where
    par = do
      string "par" <* spaces1
      syms <- betweenBrackets . sepSpace1 $ symbol
      spaces1
      betweenBrackets $ do
        idt <- identifier <* spaces1
        ss <- sepSpace1 sort
        Par syms idt ss <$> sepSpace attribute

-- | match an attribute, ignore the spaces after it,
-- input is not consumed if failed
attr :: String -> GenStrParser st ()
attr s = try $ string (':':s) >> spaces1 >> pure ()

theoryAttribute :: GenStrParser st TheoryAttribute
theoryAttribute =  attr "sorts" *> betweenBrackets (TASorts <$> sepSpace1 sortSymbolDecl)
               <|> attr "funs" *> betweenBrackets (TAFuns <$> sepSpace1 parFunSymbolDecl)
               <|> attr "sorts-description" *> (TASortsDescription <$> stringLiteral)
               <|> attr "funs-description" *> (TAFunsDescription <$> stringLiteral)
               <|> attr "definition" *> (TADefinition <$> stringLiteral)
               <|> attr "values" *> (TAValues <$> stringLiteral)
               <|> attr "notes" *> (TANotes <$> stringLiteral)
               <|> TAAttr <$> attribute
               <?> "theory attributes"

theoryDecl :: GenStrParser st TheoryDecl
theoryDecl = betweenBrackets $ do
  string "theory" <* spaces1
  s <- symbol <* spaces1
  TheoryDecl s <$> sepSpace1 theoryAttribute


-- * Logic Declarations (Sec 3.8)

logicAttribute :: GenStrParser st LogicAttribute
logicAttribute = attr "theories" *> (LATheories <$> sepSpace1 symbol)

logic :: GenStrParser st Logic
logic = betweenBrackets $ do
  string "logic" <* spaces1
  s <- symbol <* spaces1
  Logic s <$> sepSpace1 logicAttribute

