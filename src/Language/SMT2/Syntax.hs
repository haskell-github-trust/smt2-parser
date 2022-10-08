-- |
-- - Module      : Language.SMT2.Syntax
-- - Description : SMT language parser
-- - Maintainer  : ubikium@gmail.com
-- - Stability   : experimental
module Language.SMT2.Syntax where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.Text as T
import Text.Parsec.Text (GenParser)

-- * Lexicons (Sec. 3.1)

--
-- Note: semantics should be provided by specific theories.
-- See Remark 1 of the refrence.

type Numeral = T.Text

type Decimal = T.Text

type Hexadecimal = T.Text

type Binary = T.Text

type StringLiteral = T.Text

type ReservedWord = T.Text

type Symbol = T.Text

type Keyword = T.Text

-- * S-expressions (Sec. 3.2)

data SpecConstant
  = SCNumeral Numeral
  | SCDecimal Decimal
  | SCHexadecimal Hexadecimal
  | SCBinary Binary
  | SCString StringLiteral
  deriving (Eq, Show)

data SExpr
  = SEConstant SpecConstant
  | SEReservedWord ReservedWord
  | SESymbol Symbol
  | SEKeyword Keyword
  | SEList SList
  deriving (Eq, Show)

type SList = [SExpr]

-- * Identifiers (Sec 3.3)

data Index
  = IxNumeral Numeral
  | IxSymbol Symbol
  deriving (Eq, Show)

data Identifier
  = IdSymbol Symbol
  | IdIndexed Symbol (NonEmpty Index)
  deriving (Eq, Show)

-- * Attributes (Sec. 3.4)

data AttributeValue
  = AttrValSpecConstant SpecConstant
  | AttrValSymbol Symbol
  | AttrValSList SList
  deriving (Eq, Show)

data Attribute
  = AttrKey Keyword
  | AttrKeyValue Keyword AttributeValue
  deriving (Eq, Show)

-- * Sorts (Sec 3.5)

data Sort
  = SortSymbol Identifier
  | SortParameter Identifier (NonEmpty Sort)
  deriving (Eq, Show)

-- * Terms and Formulas (Sec 3.6)

data QualIdentifier
  = Unqualified Identifier
  | Qualified Identifier Sort
  deriving (Eq, Show)

data VarBinding = VarBinding Symbol Term
  deriving (Eq, Show)

data SortedVar = SortedVar Symbol Sort
  deriving (Eq, Show)

data MatchPattern
  = MPVariable Symbol
  | MPConstructor Symbol (NonEmpty Symbol)
  deriving (Eq, Show)

data MatchCase = MatchCase MatchPattern Term
  deriving (Eq, Show)

data Term
  = TermSpecConstant SpecConstant
  | TermQualIdentifier QualIdentifier
  | TermApplication QualIdentifier (NonEmpty Term)
  | TermLet (NonEmpty VarBinding) Term
  | TermForall (NonEmpty SortedVar) Term
  | TermExists (NonEmpty SortedVar) Term
  | TermMatch Term (NonEmpty MatchCase)
  | TermAnnotation Term (NonEmpty Attribute)
  deriving (Eq, Show)

-- * Theory declarations (Sec 3.7)

data SortSymbolDecl = SortSymbolDecl Identifier Numeral [Attribute]
  deriving (Eq, Show)

data MetaSpecConstant = MSC_NUMERAL | MSC_DECIMAL | MSC_STRING
  deriving (Eq, Show)

data FunSymbolDecl
  = FunConstant SpecConstant Sort [Attribute]
  | FunMeta MetaSpecConstant Sort [Attribute]
  | -- | potentially overloaded
    FunIdentifier Identifier (NonEmpty Sort) [Attribute]
  deriving (Eq, Show)

data ParFunSymbolDecl
  = -- | non-parametric
    NonPar FunSymbolDecl
  | -- | parametric
    Par (NonEmpty Symbol) Identifier (NonEmpty Sort) [Attribute]
  deriving (Eq, Show)

data TheoryAttribute
  = TASorts (NonEmpty SortSymbolDecl)
  | TAFuns (NonEmpty ParFunSymbolDecl)
  | TASortsDescription StringLiteral
  | TAFunsDescription StringLiteral
  | TADefinition StringLiteral
  | TAValues StringLiteral
  | TANotes StringLiteral
  | TAAttr Attribute
  deriving (Eq, Show)

data TheoryDecl = TheoryDecl Symbol (NonEmpty TheoryAttribute)
  deriving (Eq, Show)

-- * Logic Declarations (Sec 3.8)

data LogicAttribute
  = LATheories (NonEmpty Symbol)
  | LALanguage StringLiteral
  | LAExtensions StringLiteral
  | LAValues StringLiteral
  | LANotes StringLiteral
  | LAAttr Attribute
  deriving (Eq, Show)

data Logic = Logic Symbol (NonEmpty LogicAttribute)
  deriving (Eq, Show)

-- * Scripts (Sec 3.9)

data SortDec = SortDec Symbol Numeral
  deriving (Eq, Show)

data SelectorDec = SelectorDec Symbol Sort
  deriving (Eq, Show)

data ConstructorDec = ConstructorDec Symbol [SelectorDec]
  deriving (Eq, Show)

data DatatypeDec
  = DDNonparametric (NonEmpty ConstructorDec)
  | DDParametric (NonEmpty Symbol) (NonEmpty ConstructorDec)
  deriving (Eq, Show)

data FunctionDec = FunctionDec Symbol [SortedVar] Sort
  deriving (Eq, Show)

data FunctionDef = FunctionDef Symbol [SortedVar] Sort Term
  deriving (Eq, Show)

data PropLiteral
  = PLPositive Symbol
  | PLNegative Symbol
  deriving (Eq, Show)

data Command
  = Assert Term
  | CheckSat
  | CheckSatAssuming [PropLiteral]
  | DeclareConst Symbol Sort
  | DeclareDatatype Symbol DatatypeDec
  | -- | same number
    DeclareDatatypes (NonEmpty SortDec) (NonEmpty DatatypeDec)
  | DeclareFun Symbol [Sort] Sort
  | DeclareSort Symbol Numeral
  | DefineFun FunctionDef
  | DefineFunRec FunctionDef
  | -- | same number
    DefineFunsRec (NonEmpty FunctionDec) (NonEmpty Term)
  | DefineSort Symbol [Symbol] Sort
  | Echo StringLiteral
  | Exit
  | GetAssertions
  | GetAssignment
  | GetInfo InfoFlag
  | GetModel
  | GetOption Keyword
  | GetProof
  | GetUnsatAssumptions
  | GetUnsatCore
  | GetValue (NonEmpty Term)
  | Pop Numeral
  | Push Numeral
  | Reset
  | ResetAssertions
  | SetInfo Attribute
  | SetLogic Symbol
  | SetOption ScriptOption
  deriving (Eq, Show)

type Script = [Command]

data BValue = BTrue | BFalse
  deriving (Eq, Show)

data ScriptOption
  = DiagnosticOutputChannel StringLiteral
  | GlobalDeclarations BValue
  | InteractiveMode BValue
  | PrintSuccess BValue
  | ProduceAssertions BValue
  | ProduceAssignments BValue
  | ProduceModels BValue
  | ProduceProofs BValue
  | ProduceUnsatAssumptions BValue
  | ProduceUnsatCores BValue
  | RandomSeed Numeral
  | RegularOutputChannel StringLiteral
  | ReproducibleResourceLimit Numeral
  | Verbosity Numeral
  | OptionAttr Attribute
  deriving (Eq, Show)

data InfoFlag
  = AllStatistics
  | AssertionStackLevels
  | Authors
  | ErrorBehavior
  | Name
  | ReasonUnknown
  | Version
  | IFKeyword Keyword
  deriving (Eq, Show)

-- ** Responses (Sec 3.9.1)

data ResErrorBehavior = ImmediateExit | ContinuedExecution
  deriving (Eq, Show)

data ResReasonUnknown = Memout | Incomplete | ResReasonSExpr SExpr
  deriving (Eq, Show)

data ResModel
  = RMDefineFun FunctionDef
  | RMDefineFunRec FunctionDef
  | -- | same number
    RMDefineFunsRec (NonEmpty FunctionDec) (NonEmpty Term)

data ResInfo
  = IRErrorBehaviour ResErrorBehavior
  | IRName StringLiteral
  | IRAuthours StringLiteral
  | IRVersion StringLiteral
  | IRReasonUnknown ResReasonUnknown
  | IRAttr Attribute
  deriving (Eq, Show)

type ValuationPair = (Term, Term)

type TValuationPair = (Symbol, BValue)

data ResCheckSat = Sat | Unsat | Unknown
  deriving (Eq, Show)

-- *** instances

type CheckSatRes = GeneralRes ResCheckSat

type EchoRes = GeneralRes StringLiteral

type GetAssertionsRes = GeneralRes [Term]

type GetAssignmentRes = GeneralRes [TValuationPair]

type GetInfoRes = GeneralRes (NonEmpty ResInfo)

type GetModelRes = GeneralRes ResModel

type GetOptionRes = GeneralRes AttributeValue

type GetProofRes = GeneralRes SExpr

type GetUnsatAssumpRes = GeneralRes [Symbol]

type GetUnsatCoreRes = GeneralRes [Symbol]

type GetValueRes = GeneralRes (NonEmpty ValuationPair)

data GeneralRes res
  = ResSuccess
  | ResSpecific res
  | ResUnsupported
  | ResError StringLiteral
  deriving (Eq, Show)

class SpecificSuccessRes s where
  specificSuccessRes :: GenParser st s
