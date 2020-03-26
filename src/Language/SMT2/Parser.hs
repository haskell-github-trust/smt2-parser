{-|
 - Module      : Language.SMT2.Parser
 - Description : SMT language parser
 - Maintainer  : yokis1997@pku.edu.cn
 - Stability   : experimental
-}
{-# LANGUAGE FlexibleInstances #-}
module Language.SMT2.Parser
  ( -- * Utils
    -- $utils
    parseString
  , parseStringEof
  , strip
  , removeComment
    -- * Lexicons (Sec. 3.1)
    -- $lexicon
  , numeral
  , decimal
  , hexadecimal
  , binary
  , stringLiteral
  , reservedWord
  , symbol
  , keyword
    -- * S-expressions (Sec. 3.2)
  , slist
  , specConstant
  , sexpr
    -- * Identifiers (Sec 3.3)
  , index
  , identifier
    -- * Attributes (Sec. 3.4)
  , attributeValue
  , attribute
   -- * Sorts (Sec 3.5)
  , sort
    -- * Terms and Formulas (Sec 3.6)
  , qualIdentifier
  , varBinding
  , sortedVar
  , matchPattern
  , matchCase
  , term
    -- * Theory declarations (Sec 3.7)
  , sortSymbolDecl
  , metaSpecConstant
  , funSymbolDecl
  , parFunSymbolDecl
  , theoryAttribute
  , theoryDecl
    -- * Logic Declarations (Sec 3.8)
  , logicAttribute
  , logic
    -- * Scripts (Sec 3.9)
  , sortDec
  , selectorDec
  , constructorDec
  , datatypeDec
  , functionDec
  , functionDef
  , propLiteral
  , command
  , script
  , bValue
  , scriptOption
  , infoFlag
    -- ** Responses (Sec 3.9.1)
    -- *** values
  , resErrorBehaviour
  , resReasonUnknown
  , resModel
  , resInfo
  , valuationPair
  , tValuationPair
  , resCheckSat
    -- *** instances
  , checkSatRes
  , echoRes
  , getAssertionsRes
  , getAssignmentRes
  , getInfoRes
  , getModelRes
  , getOptionRes
  , getProofRes
  , getUnsatAssumpRes
  , getUnsatCoreRes
  , getValueRes
  ) where

import           Data.Char              (toLower)
import           Data.Functor           (($>))
import           Data.List.NonEmpty     (NonEmpty, fromList)
import           Language.SMT2.Syntax
import           Text.Parsec            (ParseError, eof, parse, try)
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Prim       (many, unexpected, (<?>), (<|>))
import           Text.Parsec.String     (Parser)

parseString :: Parser a -> String -> Either ParseError a
parseString p = parse p ""

parseStringEof :: Parser a -> String -> Either ParseError a
parseStringEof p = parse (p <* eof) ""

-- * Utils
-- $utils
-- commonly used combinators

-- | skip one or more @spaces@
spaces1 :: GenStrParser st ()
spaces1 = skipMany1 space

-- | between round brackets
betweenBrackets :: GenStrParser st a -> GenStrParser st a
betweenBrackets = try . between (char '(' <* spaces) (spaces *> char ')')

-- | @many p@, separated by @spaces1@, possibly has a trailing @spaces1@
sepSpace :: GenStrParser st a -> GenStrParser st [a]
sepSpace p = sepEndBy p spaces1

-- | @many1 p@, separated by @spaces1@, possibly has a trailing @spaces1@
sepSpace1 :: GenStrParser st a -> GenStrParser st (NonEmpty a)
sepSpace1 p = fromList <$> sepEndBy1 p spaces1

-- | @many p@, separated by @spaces@, possibly has a trailing @spaces@
sepOptSpace :: GenStrParser st a -> GenStrParser st [a]
sepOptSpace p = sepEndBy p spaces

-- | @many1 p@, separated by @spaces@, possibly has a trailing @spaces@
sepOptSpace1 :: GenStrParser st a -> GenStrParser st (NonEmpty a)
sepOptSpace1 p = fromList <$> sepEndBy1 p spaces

-- | match an string, ignore @spaces@ after,
-- input is not consumed if failed
tryStr :: String -> GenStrParser st ()
tryStr s = try $ string s *> spaces $> ()

-- | match an string, must have @spaces1@ after, ignore them,
-- input is not consumed if failed
tryStr1 :: String -> GenStrParser st ()
tryStr1 s = try $ string s *> spaces1 $> ()

-- | like @tryStr@, but prefix with a @':'@
tryAttr :: String -> GenStrParser st ()
tryAttr s = tryStr (':':s)

-- | like @tryStr1@, but prefix with a @':'@
tryAttr1 :: String -> GenStrParser st ()
tryAttr1 s = tryStr1 (':':s)

-- | strip away the leading and trailing @spaces@
strip :: GenStrParser st a -> GenStrParser st a
strip p = spaces *> p <* spaces

-- | remove comments
removeComment :: String -> String
removeComment = rc ""
  where
    rc acc "" = acc
    rc acc (c:cs) = let f = case c of
                              '"' -> capture "\"" ('"':)
                              '|' -> capture "|" ('|':)
                              ';' -> capture "\n\r" $ const " "
                              x   -> nextPos x
                     in f acc cs
    capture stops after acc cs = let (captured, rest) = break (`elem` stops) cs
                                  in case rest of
                                       [] -> acc <> after captured
                                       (s:ss) -> rc (acc <> after captured <> [s]) ss
    -- 1. if the string is ill-formed, ignore;
    --    let the parser catch, for a better format
    -- 2. the double " escaping in a string literal is the same as capturing twice
    -- 3. for comments ended with \n\r or \r\n, the second is left
    nextPos x acc = rc (x `snoc` acc)
    snoc c cs = cs <> [c]


-- * Lexicons (Sec. 3.1)
-- $lexicon
-- Parsers for lexicons.
--
-- For a numeral, a decimal, or a string literal, the parsed result is the same.
-- For a hexadecimal or a binary, the result is stripped with marks (@#x@ and @#b@).

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
    nonEscaped = noneOf "\""
    escaped = try (string "\"\"") $> '"'

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

-- |  enclosing a simple symbol in vertical bars does not produce a
-- new symbol, e.g. @abc@ and @|abc|@ are the /same/ symbol
-- this is guaranteed by removing the bars
symbol :: GenStrParser st Symbol
symbol =  notFollowedBy (try reservedWord) >> (quotedSymbol <|> simpleSymbol <?> "symbol")
  where
    simpleSymbol = do
      c <- nameChar <|> letter
      cs <- many (alphaNum <|> nameChar)
      return (c:cs)
    quotedSymbol = between (char '|') (char '|') $ many (noneOf "\\|")

keyword :: GenStrParser st Keyword
keyword = do char ':'
             many1 (alphaNum <|> nameChar)


-- * S-expressions (Sec. 3.2)

slist :: GenStrParser st SList
slist = betweenBrackets . sepSpace $ sexpr

-- | a constant must be followed by a space to delimit the end
specConstant :: GenStrParser st SpecConstant
specConstant =  SCDecimal <$> try decimal  -- numeral can be a prefix
            <|> SCNumeral <$> try numeral
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

index :: GenStrParser st Index
index =  IxNumeral <$> numeral
     <|> IxSymbol <$> symbol

identifier :: GenStrParser st Identifier
identifier =  IdSymbol <$> symbol -- symbol cannot start with (, so no ambiguity
          <|> idIndexed
          <?> "identifier"
  where
    idIndexed = betweenBrackets $ do
      char '_' <* spaces1
      s <- symbol <* spaces1
      IdIndexed s <$> sepSpace1 index


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
      tryStr "as"
      id <- identifier <* spaces1
      Qualified id <$> sort

varBinding :: GenStrParser st VarBinding
varBinding = betweenBrackets $ VarBinding <$> symbol <* spaces1 <*> term

sortedVar :: GenStrParser st SortedVar
sortedVar = betweenBrackets $ SortedVar <$> symbol <* spaces1 <*> sort

matchPattern :: GenStrParser st MatchPattern
matchPattern =  MPVariable <$> symbol
       <|> mPConstructor
       <?> "pattern"
  where
    mPConstructor = betweenBrackets $ do
      c <- symbol <* spaces1
      MPConstructor c <$> sepSpace1 symbol

matchCase :: GenStrParser st MatchCase
matchCase = betweenBrackets $ do
  p <- matchPattern <* spaces1
  MatchCase p <$> term

term :: GenStrParser st Term
term =  TermSpecConstant <$> try specConstant
    <|> TermQualIdentifier <$> try qualIdentifier
    <|> try application
    <|> try binding
    <|> try quantifyForall
    <|> try quantifyExists
    <|> try match
    <|> try annotation
    <?> "term"
  where
    application = betweenBrackets $ do
      id <- qualIdentifier <* spaces
      TermApplication id <$> sepOptSpace1 term
    binding = betweenBrackets $ do
      tryStr "let"
      vbs <- betweenBrackets $ sepOptSpace1 varBinding
      space
      TermLet vbs <$> term
    quantifyForall = betweenBrackets $ do
      tryStr "forall"
      svs <- betweenBrackets $ sepOptSpace1 sortedVar
      spaces
      TermForall svs <$> term
    quantifyExists = betweenBrackets $ do
      tryStr "exists"
      svs <- betweenBrackets $ sepOptSpace1 sortedVar
      spaces
      TermExists svs <$> term
    match = betweenBrackets $ do
      tryStr "match"
      t <- term <* spaces
      TermMatch t <$> betweenBrackets (sepOptSpace1 matchCase)
    annotation = betweenBrackets $ do
      char '!' <* spaces1
      t <- term <* spaces1
      TermAnnotation t <$> sepSpace1 attribute


-- * Theory declarations (Sec 3.7)

sortSymbolDecl :: GenStrParser st SortSymbolDecl
sortSymbolDecl = betweenBrackets $ do
  i <- identifier <* spaces1
  n <- numeral <* spaces -- if no attribute, don't need space
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
      tryStr "par"
      syms <- betweenBrackets . sepSpace1 $ symbol
      spaces1
      betweenBrackets $ do
        idt <- identifier <* spaces1
        ss <- sepSpace1 sort
        Par syms idt ss <$> sepSpace attribute


theoryAttribute :: GenStrParser st TheoryAttribute
theoryAttribute =  tryAttr "sorts" *> betweenBrackets (TASorts <$> sepSpace1 sortSymbolDecl)
               <|> tryAttr "funs" *> betweenBrackets (TAFuns <$> sepSpace1 parFunSymbolDecl)
               <|> tryAttr "sorts-description" *> (TASortsDescription <$> stringLiteral)
               <|> tryAttr "funs-description" *> (TAFunsDescription <$> stringLiteral)
               <|> tryAttr "definition" *> (TADefinition <$> stringLiteral)
               <|> tryAttr "values" *> (TAValues <$> stringLiteral)
               <|> tryAttr "notes" *> (TANotes <$> stringLiteral)
               <|> TAAttr <$> attribute
               <?> "theory attributes"

theoryDecl :: GenStrParser st TheoryDecl
theoryDecl = betweenBrackets $ do
  tryStr "theory"
  s <- symbol <* spaces1
  TheoryDecl s <$> sepSpace1 theoryAttribute


-- * Logic Declarations (Sec 3.8)

logicAttribute :: GenStrParser st LogicAttribute
logicAttribute =  tryAttr "theories" *> betweenBrackets (LATheories <$> sepSpace1 symbol)
              <|> tryAttr "language" *> (LALanguage <$> stringLiteral)
              <|> tryAttr "extensions" *> (LAExtensions <$> stringLiteral)
              <|> tryAttr "values" *> (LAValues <$> stringLiteral)
              <|> tryAttr "notes" *> (LANotes <$> stringLiteral)
              <|> LAAttr <$> attribute

logic :: GenStrParser st Logic
logic = betweenBrackets $ do
  tryStr "logic"
  s <- symbol <* spaces1
  Logic s <$> sepSpace1 logicAttribute


-- * Scripts (Sec 3.9)

sortDec :: GenStrParser st SortDec
sortDec = betweenBrackets $ do
  s <- symbol <* spaces1
  SortDec s <$> numeral

selectorDec :: GenStrParser st SelectorDec
selectorDec = betweenBrackets $ do
  s <- symbol <* spaces1
  SelectorDec s <$> sort

constructorDec :: GenStrParser st ConstructorDec
constructorDec = betweenBrackets $ do
  s <- symbol <* spaces1
  ConstructorDec s <$> sepSpace selectorDec

datatypeDec :: GenStrParser st DatatypeDec
datatypeDec =  DDNonparametric <$> betweenBrackets (sepSpace1 constructorDec)
           <|> parametric
           <?> "datatype declaration"
  where
    parametric = betweenBrackets $ do
      tryStr1 "par"
      ss <- betweenBrackets $ sepSpace1 symbol
      spaces
      DDParametric ss <$> betweenBrackets (sepSpace1 constructorDec)

functionDec :: GenStrParser st FunctionDec
functionDec = betweenBrackets $ do
  s <- symbol <* spaces
  svs <- betweenBrackets $ sepSpace sortedVar
  spaces
  FunctionDec s svs <$> sort

functionDef :: GenStrParser st FunctionDef
functionDef = betweenBrackets $ do
  s <- symbol <* spaces
  svs <- betweenBrackets $ sepSpace sortedVar
  t <- spaces *> sort <* spaces1
  FunctionDef s svs t <$> term

propLiteral :: GenStrParser st PropLiteral
propLiteral =  PLPositive <$> symbol
           <|> PLNegative <$> betweenBrackets (tryStr1 "not" *> symbol)

command :: GenStrParser st Command
command =  cmd "assert" (Assert <$> term)
       <|> cmd "check-sat" (pure CheckSat)
       <|> cmd "check-sat-assuming" (betweenBrackets (CheckSatAssuming <$> sepSpace propLiteral))
       <|> cmd "declare-const" (DeclareConst <$> (symbol <* spaces1) <*> sort)
       <|> cmd "declare-datatype" (DeclareDatatype <$> (symbol <* spaces1) <*> datatypeDec)
       <|> cmd "declare-datatypes" declareDatatypes
       <|> cmd "declare-fun" declareFun
       <|> cmd "declare-sort" declareSort
       <|> cmd "define-fun" (DefineFun <$> functionDef)
       <|> cmd "define-fun-rec" (DefineFunRec <$> functionDef)
       <|> cmd "define-funs-rec" defineFunsRec
       <|> cmd "define-sort" defineSort
       <|> cmd "echo" (Echo <$> stringLiteral)
       <|> cmd "exit" (pure Exit)
       <|> cmd "get-assertions" (pure GetAssertions)
       <|> cmd "get-assignment" (pure GetAssignment)
       <|> cmd "get-info" (GetInfo <$> infoFlag)
       <|> cmd "get-model" (pure GetModel)
       <|> cmd "get-option" (GetOption <$> keyword)
       <|> cmd "get-proof" (pure GetProof)
       <|> cmd "get-unsat-assumptions" (pure GetUnsatAssumptions)
       <|> cmd "get-unsat-core" (pure GetUnsatCore)
       <|> cmd "get-value" (GetValue <$> getValue)
       <|> cmd "pop" (Pop <$> numeral)
       <|> cmd "push" (Push <$> numeral)
       <|> cmd "reset" (pure Reset)
       <|> cmd "reset-assertions" (pure ResetAssertions)
       <|> cmd "set-info" (SetInfo <$> attribute)
       <|> cmd "set-logic" (SetLogic <$> symbol)
       <|> cmd "set-option" (SetOption <$> scriptOption)
       <?> "command"
  where
    cmd s p = try $ betweenBrackets (tryStr s *> p)
    declareDatatypes = do
      sds <- betweenBrackets $ sepSpace1 sortDec
      spaces
      dds <- betweenBrackets $ sepSpace1 datatypeDec
      if length sds == length dds
         then pure $ DeclareDatatypes sds dds
         else unexpected "declare-datatypes: sorts and datatypes should have same length"
    declareFun = do
      s <- symbol <* spaces1
      ss <- betweenBrackets $ sepSpace sort
      spaces1
      DeclareFun s ss <$> sort
    declareSort = do
      s <- symbol <* spaces1
      DeclareSort s <$> numeral
    defineFunsRec = do
      fds <- betweenBrackets $ sepSpace1 functionDec
      spaces
      ts <- betweenBrackets $ sepSpace1 term
      if length fds == length ts
         then pure $ DefineFunsRec fds ts
         else unexpected "define-funs-rec: declarations and terms should have same length"
    defineSort = do
      s <- symbol <* spaces1
      ss <- betweenBrackets $ sepSpace symbol
      spaces1
      DefineSort s ss <$> sort
    getValue = betweenBrackets $ sepSpace1 term

script :: GenStrParser st Script
script = spaces *> sepOptSpace command <* spaces

bValue :: GenStrParser st BValue
bValue =  string "true" $> BTrue
      <|> string "false" $> BFalse
      <?> "bool value"

scriptOption :: GenStrParser st ScriptOption
scriptOption =  DiagnosticOutputChannel <$> opt "diagnostic-output-channel" stringLiteral
            <|> GlobalDeclarations <$> optB "global-declarations"
            <|> InteractiveMode <$> optB "interactive-mode"
            <|> PrintSuccess <$> optB "print-success"
            <|> ProduceAssertions <$> optB "produce-assertions"
            <|> ProduceAssignments <$> optB "produce-assignments"
            <|> ProduceModels <$> optB "produce-models"
            <|> ProduceProofs <$> optB "produce-proofs"
            <|> ProduceUnsatAssumptions <$> optB "produce-unsat-assumptions"
            <|> ProduceUnsatCores <$> optB "produce-unsat-cores"
            <|> RandomSeed <$> opt "random-seed" numeral
            <|> RegularOutputChannel <$> opt "regular-output-channel" stringLiteral
            <|> ReproducibleResourceLimit <$> opt "reproducible-resource-limit" numeral
            <|> Verbosity <$> opt "verbosity" numeral
            <|> OptionAttr <$> attribute
            <?> "script option"
  where
    opt s p = tryAttr s *> spaces1 *> p
    optB s = opt s bValue

infoFlag :: GenStrParser st InfoFlag
infoFlag =  tryAttr "all-statistics" $> AllStatistics
        <|> tryAttr "assertion-stack-levels" $> AssertionStackLevels
        <|> tryAttr "authors" $> Authors
        <|> tryAttr "error-behavior" $> ErrorBehavior
        <|> tryAttr "name" $> Name
        <|> tryAttr "reason-unknown" $> ReasonUnknown
        <|> tryAttr "version" $> Version
        <|> IFKeyword <$> keyword
        <?> "info flag"

-- ** Responses (Sec 3.9.1)

genRes :: SpecificSuccessRes res => GenStrParser st (GeneralRes res)
genRes =  tryStr "success" $> ResSuccess
      <|> ResSpecific <$> specificSuccessRes
      <|> tryStr "unsupported" $> ResUnsupported
      <|> ResError <$> betweenBrackets (tryStr "error" *> spaces1 *> stringLiteral)

resErrorBehaviour :: GenStrParser st ResErrorBehavior
resErrorBehaviour =  tryStr "immediate-exit" $> ImmediateExit
                 <|> tryStr "continued-execution" $> ContinuedExecution
                 <?> "response error behavior"

resReasonUnknown :: GenStrParser st ResReasonUnknown
resReasonUnknown =  tryStr "memout" $> Memout
                <|> tryStr "incomplete" $> Incomplete
                <?> "response reason unknown"

resModel :: GenStrParser st ResModel
resModel =  def "define-fun" (RMDefineFun <$> functionDef)
        <|> def "define-fun-rec" (RMDefineFunRec <$> functionDef)
        <|> def "define-funs-rec" rMDefineFunsRec
  where
    def s p = betweenBrackets $ tryStr s *> p
    rMDefineFunsRec = do
      fdcs <- betweenBrackets $ sepSpace1 functionDec
      spaces
      ts <- betweenBrackets $ sepSpace1 term
      if length fdcs == length ts
         then pure $ RMDefineFunsRec fdcs ts
         else unexpected "get-model response, declarations and terms should have same length"

instance SpecificSuccessRes ResModel where
  specificSuccessRes = resModel

tValuationPair :: GenStrParser st TValuationPair
tValuationPair = betweenBrackets $ do
  s <- symbol <* spaces1
  b <- bValue
  return (s, b)

resCheckSat :: GenStrParser st ResCheckSat
resCheckSat =  tryStr "sat" $> Sat
           <|> tryStr "unsat" $> Unsat
           <|> tryStr "unknown" $> Unknown

instance SpecificSuccessRes ResCheckSat where
  specificSuccessRes = resCheckSat

resInfo :: GenStrParser st ResInfo
resInfo =  IRErrorBehaviour <$> (tryAttr "error-behavior" *> resErrorBehaviour)
       <|> IRName <$> (tryAttr "name" *> stringLiteral)
       <|> IRAuthours <$> (tryAttr "authors" *> stringLiteral)
       <|> IRVersion <$> (tryAttr "version" *> stringLiteral)
       <|> IRReasonUnknown <$> (tryAttr "reason-unknown" *> resReasonUnknown)
       <|> IRAttr <$> attribute

instance SpecificSuccessRes (NonEmpty ResInfo) where
  specificSuccessRes = betweenBrackets $ sepSpace1 resInfo

-- *** instances

checkSatRes :: GenStrParser st CheckSatRes
checkSatRes = genRes

instance SpecificSuccessRes StringLiteral where
  specificSuccessRes = stringLiteral

echoRes :: GenStrParser st EchoRes
echoRes = genRes

instance SpecificSuccessRes [Term] where
  specificSuccessRes = betweenBrackets $ sepSpace term

getAssertionsRes :: GenStrParser st GetAssertionsRes
getAssertionsRes = genRes

instance SpecificSuccessRes [TValuationPair] where
  specificSuccessRes = betweenBrackets $ sepSpace tValuationPair

getAssignmentRes :: GenStrParser st GetAssignmentRes
getAssignmentRes = genRes

getInfoRes :: GenStrParser st GetInfoRes
getInfoRes = genRes

getModelRes :: GenStrParser st GetModelRes
getModelRes = genRes

instance SpecificSuccessRes AttributeValue where
  specificSuccessRes = attributeValue

getOptionRes :: GenStrParser st GetOptionRes
getOptionRes = genRes

instance SpecificSuccessRes SExpr where
  specificSuccessRes = sexpr

getProofRes :: GenStrParser st GetProofRes
getProofRes = genRes

instance SpecificSuccessRes [Symbol] where
  specificSuccessRes = betweenBrackets $ sepSpace symbol

getUnsatAssumpRes :: GenStrParser st GetUnsatAssumpRes
getUnsatAssumpRes = genRes

getUnsatCoreRes :: GenStrParser st GetUnsatCoreRes
getUnsatCoreRes = genRes

valuationPair :: GenStrParser st ValuationPair
valuationPair = betweenBrackets $ do
  t1 <- term <* spaces1
  t2 <- term
  return (t1, t2)

instance SpecificSuccessRes (NonEmpty ValuationPair) where
  specificSuccessRes = betweenBrackets $ sepSpace1 valuationPair

getValueRes :: GenStrParser st GetValueRes
getValueRes = genRes

