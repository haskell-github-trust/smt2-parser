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
                | IdIndexed Symbol (NonEmpty Numeral)
  deriving (Eq, Show)

idIndexed :: GenStrParser st Identifier
idIndexed = betweenBrackets $ do
  char '_' <* spaces1
  s <- symbol <* spaces1
  ns <- sepEndBy1 numeral spaces1
  return $ IdIndexed s (fromList ns)


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
         <?> "attribute"

-- * Sorts (Sec 3.5)

data Sort = SortSymbol Identifier
          | SortParameter Identifier (NonEmpty Sort)
  deriving (Eq, Show)

sortParameter :: GenStrParser st Sort
sortParameter = betweenBrackets $ do
  i <- identifier <* spaces1
  ss <- sepEndBy1 sort spaces1
  return $ SortParameter i (fromList ss)

sort :: GenStrParser st Sort
sort =  SortSymbol <$> try identifier
    <|> sortParameter
    <?> "sort"

-- * Terms and Formulas (Sec 3.6)

data QualIdentifier = Unqualified Identifier
                    | Qualified Identifier Sort
  deriving (Eq, Show)

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

data VarBinding = VarBinding Symbol Term
  deriving (Eq, Show)

varBinding :: GenStrParser st VarBinding
varBinding = betweenBrackets $ VarBinding <$> symbol <* spaces1 <*> term

data SortedVar = SortedVar Symbol Sort
  deriving (Eq, Show)

sortedVar :: GenStrParser st SortedVar
sortedVar = betweenBrackets $ SortedVar <$> symbol <* spaces1 <*> sort

data Term = TermSpecConstant SpecConstant
          | TermQualIdentifier QualIdentifier
          | TermApplication QualIdentifier (NonEmpty Term)
          | TermLet (NonEmpty VarBinding) Term
          | TermForall (NonEmpty SortedVar) Term
          | TermExists (NonEmpty SortedVar) Term
          | TermAnnotation Term (NonEmpty Attribute) -- ^ only attributes, do not support e.g. @:pattern terms@
  deriving (Eq, Show)

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
      ts <- sepEndBy1 term spaces1
      return $ TermApplication id (fromList ts)
    binding = betweenBrackets $ do
      string "let" <* spaces1
      vbs <- betweenBrackets $ sepEndBy1 varBinding spaces1
      spaces1
      TermLet (fromList vbs) <$> term
    quantifyForall = betweenBrackets $ do
      string "forall" <* spaces1
      svs <- betweenBrackets $ sepEndBy1 sortedVar spaces1
      spaces1
      TermForall (fromList svs) <$> term
    quantifyExists = betweenBrackets $ do
      string "exists" <* spaces1
      svs <- betweenBrackets $ sepEndBy1 sortedVar spaces1
      spaces1
      TermExists (fromList svs) <$> term
    annotation = betweenBrackets $ do
      char '!' <* spaces1
      t <- term <* spaces1
      attrs <- sepEndBy1 attribute spaces1
      return $ TermAnnotation t (fromList attrs)

-- * Theory declarations (Sec 3.7)

data SortSymbolDecl = SortSymbolDecl Identifier Numeral [Attribute]

data MetaSpecConstant = MSC_NUMERAL | MSC_DECIMAL | MSC_STRING

data FunSymbolDecl = FunConstant SpecConstant Sort [Attribute]
                   | FunMeta MetaSpecConstant Sort [Attribute]
                   | FunIdentifier Identifier (NonEmpty Sort) [Attribute]  -- ^ potentially overloaded

data ParFunSymbolDecl = NonPar FunSymbolDecl -- ^ non-parametric
                      | Par (NonEmpty Symbol) Identifier (NonEmpty Sort) [Attribute] -- ^ parametric

data TheoryAttribute = TASorts (NonEmpty SortSymbolDecl)
                     | TAFuns (NonEmpty ParFunSymbolDecl)
                     | TASortsDescription String
                     | TAFunsDescription String
                     | TADefinition String
                     | TAValues String
                     | TANotes String
                     | TAAttr Attribute

data TheoryDecl = TheoryDecl Symbol (NonEmpty TheoryAttribute)

sortSymbolDecl :: GenStrParser st SortSymbolDecl
sortSymbolDecl = betweenBrackets $ do
  i <- identifier <* spaces1
  n <- numeral <* spaces1
  as <- sepEndBy attribute spaces1
  return $ SortSymbolDecl i n as

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
      as <- sepEndBy attribute spaces1
      return $ FunConstant sc s as
    funMeta = do
      m <- metaSpecConstant <* spaces1
      s <- sort <* spaces1
      as <- sepEndBy attribute spaces1
      return $ FunMeta m s as
    funIdentifier = do
      i <- identifier <* spaces1
      ss <- sepEndBy1 sort spaces1
      as <- sepEndBy attribute spaces1
      return $ FunIdentifier i (fromList ss) as

parFunSymbolDecl :: GenStrParser st ParFunSymbolDecl
parFunSymbolDecl =  NonPar <$> funSymbolDecl
                <|> betweenBrackets par
                <?> "potentially parametric function symbol declaration"
  where
    par = do
      string "par" <* spaces1
      syms <- betweenBrackets $ sepEndBy1 symbol spaces1
      spaces1
      betweenBrackets $ do
        idt <- identifier <* spaces1
        ss <- sepEndBy1 sort spaces1
        as <- sepEndBy attribute spaces1
        return $ Par (fromList syms) idt (fromList ss) as

theoryAttribute :: GenStrParser st TheoryAttribute
theoryAttribute =  attr "sorts" *> betweenBrackets (TASorts . fromList <$> sepEndBy1 sortSymbolDecl spaces1)
               <|> attr "funs" *> betweenBrackets (TAFuns . fromList <$> sepEndBy1 parFunSymbolDecl spaces1)
               <|> attr "sorts-description" *> (TASortsDescription <$> stringLiteral)
               <|> attr "funs-description" *> (TAFunsDescription <$> stringLiteral)
               <|> attr "definition" *> (TADefinition <$> stringLiteral)
               <|> attr "values" *> (TAValues <$> stringLiteral)
               <|> attr "notes" *> (TANotes <$> stringLiteral)
               <|> TAAttr <$> attribute
               <?> "theory attributes"
  where
    attr :: String -> GenStrParser st ()
    attr s = try $ string (':':s) >> spaces1 >> pure ()

theoryDecl :: GenStrParser st TheoryDecl
theoryDecl = betweenBrackets $ do
  string "theory" <* spaces1
  s <- symbol <* spaces1
  as <- sepEndBy1 theoryAttribute spaces1
  return $ TheoryDecl s (fromList as)
