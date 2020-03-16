{-|
 - Module      : Language.SMT2.Syntax
 - Description : SMT language parser
 - Maintainer  : yokis1997@pku.edu.cn
 - Stability   : experimental
-}
module Language.SMT2.Syntax where

import           Data.List.NonEmpty (NonEmpty)
import           Text.Parsec        (Parsec)

type GenStrParser st = Parsec String st

-- * Lexicons (Sec. 3.1)
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


-- * Identifiers (Sec 3.3)

data Identifier = IdSymbol Symbol
                | IdIndexed Symbol (NonEmpty Numeral)
  deriving (Eq, Show)


-- * Attributes (Sec. 3.4)

data AttributeValue = AttrValSpecConstant SpecConstant
                    | AttrValSymbol Symbol
                    | AttrValSList SList
  deriving (Eq, Show)

data Attribute = AttrKey Keyword
               | AttrKeyValue Keyword AttributeValue
  deriving (Eq, Show)


-- * Sorts (Sec 3.5)

data Sort = SortSymbol Identifier
          | SortParameter Identifier (NonEmpty Sort)
  deriving (Eq, Show)


-- * Terms and Formulas (Sec 3.6)

data QualIdentifier = Unqualified Identifier
                    | Qualified Identifier Sort
  deriving (Eq, Show)

data VarBinding = VarBinding Symbol Term
  deriving (Eq, Show)

data SortedVar = SortedVar Symbol Sort
  deriving (Eq, Show)

data Term = TermSpecConstant SpecConstant
          | TermQualIdentifier QualIdentifier
          | TermApplication QualIdentifier (NonEmpty Term)
          | TermLet (NonEmpty VarBinding) Term
          | TermForall (NonEmpty SortedVar) Term
          | TermExists (NonEmpty SortedVar) Term
          | TermAnnotation Term (NonEmpty Attribute)
            -- ^ only attributes, do not support e.g. @:pattern terms@
  deriving (Eq, Show)


-- * Theory declarations (Sec 3.7)

data SortSymbolDecl = SortSymbolDecl Identifier Numeral [Attribute]

data MetaSpecConstant = MSC_NUMERAL | MSC_DECIMAL | MSC_STRING

data FunSymbolDecl = FunConstant SpecConstant Sort [Attribute]
                   | FunMeta MetaSpecConstant Sort [Attribute]
                   | FunIdentifier Identifier (NonEmpty Sort) [Attribute]
                     -- ^ potentially overloaded

data ParFunSymbolDecl = NonPar FunSymbolDecl
                        -- ^ non-parametric
                      | Par (NonEmpty Symbol) Identifier (NonEmpty Sort) [Attribute]
                        -- ^ parametric

data TheoryAttribute = TASorts (NonEmpty SortSymbolDecl)
                     | TAFuns (NonEmpty ParFunSymbolDecl)
                     | TASortsDescription String
                     | TAFunsDescription String
                     | TADefinition String
                     | TAValues String
                     | TANotes String
                     | TAAttr Attribute

data TheoryDecl = TheoryDecl Symbol (NonEmpty TheoryAttribute)


-- * Logic Declarations (Sec 3.8)

data LogicAttribute = LATheories (NonEmpty Symbol)
                    | LALanguage String
                    | LAExtensions String
                    | LAValues String
                    | LANotes String
                    | LAAttr Attribute

data Logic = Logic Symbol (NonEmpty LogicAttribute)


-- * Scripts (Sec 3.9)

data Command = SetLogic Symbol
             | SetOption ScriptOption
             | SetInfo Attribute
             | DeclareSort Symbol Numeral
             | DefineSort Symbol [Symbol] Sort
             | DeclareFun Symbol [Sort] Sort
             | DefineFun Symbol [SortedVar] Sort Term
             | Push Numeral
             | Pop Numeral
             | Assert Term
             | CheckSat
             | GetAssertions
             | GetProof
             | GetUnsatCore
             | GetValue (NonEmpty Term)
             | GetAssignment
             | GetOption Keyword
             | GetInfo InfoFlag
             | Exit

type Script = [Command]

data BValue = BTrue | BFalse

data ScriptOption = PrintSuccess BValue
                  | ExpandDefinitions BValue
                  | InteractiveMode BValue
                  | ProduceProofs BValue
                  | ProduceUnsatCores BValue
                  | ProduceModels BValue
                  | ProduceAssignments BValue
                  | RegularOutputChannel StringLiteral
                  | DiagnosticOutputChannel StringLiteral
                  | RandomSeed Numeral
                  | Verbosity Numeral
                  | OptionAttr Attribute

data InfoFlag = ErrorBehavior | Name | Authours | Version
              | Status | ReasonUnknown
              | IFKeyword Keyword
              | AllStatistics

-- ** Responses

data GenRes success = ResUnsupported | ResSuccess success | ResError StringLiteral

class ResParsable s where
  resParser :: GenStrParser st s

data ResErrorBehavior = ImmediateExit | ContinuedExecution

data ResReasonUnknown = Memout | Incomplete

data ResStatus = Sat | Unsat | Unknown

data InfoResponse = IRErrorBehaviour ResErrorBehavior
                  | IRName StringLiteral
                  | IRAuthours StringLiteral
                  | IRVersion StringLiteral
                  | IRReasonUnknown ResReasonUnknown
                  | IRAttr Attribute

-- *** instances

type GetInfoRes = GenRes (NonEmpty InfoResponse)

type CheckSatRes = GenRes ResStatus

type GetAssertionsRes = GenRes [Term]

type Proof = SExpr
type GetProofRes = GenRes Proof

type GetUnsatCoreRes = GenRes [Symbol]

type ValuationPair = (Term, Term)
type GetValueRes = GenRes (NonEmpty ValuationPair)

type TValuationPair = (Symbol, BValue)
type GetAssignmentRes = GenRes [TValuationPair]

type GetOptionRes = GenRes AttributeValue

