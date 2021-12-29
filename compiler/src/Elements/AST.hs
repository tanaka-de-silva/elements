module Elements.AST where

import           Data.Aeson                     ( ToJSON )
import           Data.Hashable                  ( Hashable )
import           Data.String                    ( IsString(..) )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           GHC.Generics                   ( Generic )
import           GHC.Int                        ( Int32
                                                , Int64
                                                )

data NumericValue = IntValue Int32
                  | LongValue Int64
                  | DoubleValue Double
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data ArithmeticOp = Add
                  | Subtract
                  | Multiply
                  | Divide
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data BinaryArithOp' = BinaryArithOp'
  { binArithOp   :: ArithmeticOp
  , binAritOpLhs :: Expression
  , binAritOpRhs :: Expression
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

data LogicalOp = And
               | Or
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data BinaryLogicalOp' = BinaryLogicalOp'
  { binLogicalOp    :: LogicalOp
  , binLogicalOpLhs :: Expression
  , binLogicakOpRhs :: Expression
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

data ComparisonOp = LessThan
                  | LessThanOrEquals
                  | Equals
                  | GreaterThanOrEquals
                  | GreaterThan
                  | NotEquals
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data Comparison' = Comparison'
  { cmpOp  :: ComparisonOp
  , cmpLhs :: Expression
  , cmpRhs :: Expression
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

data IfElse' = IfElse'
  { testCondition :: Expression
  , thenExpr      :: Expression
  , elseExpr      :: Expression
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

newtype Identifier = Identifier { unwrapIndentifier :: Text }
  deriving newtype (Eq, Hashable, Show, ToJSON)

instance IsString Identifier where
  fromString = Identifier . Text.pack

newtype LineNumber = LineNumber { unwrapLineNumber :: Int }
  deriving newtype (Eq, Show, Num, ToJSON)

data ValBinding' = ValBinding'
  { vbIdentifier :: Identifier
  , vbLineNum    :: LineNumber
  , vbBoundExpr  :: Expression
  , vbBaseExpr   :: Expression
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

data Value' = Value'
  { vIdentifier :: Identifier
  , vLineNum    :: LineNumber
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

data Expression = NumericLiteral NumericValue
                | BoolLiteral Bool
                | Value Value'
                | UnaryMinus Expression
                | BinaryArithOp BinaryArithOp'
                | BinaryLogicalOp BinaryLogicalOp'
                | Comparison Comparison'
                | IfElse IfElse'
                | ValBinding ValBinding'
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

value :: Identifier -> LineNumber -> Expression
value i l = Value $ Value' i l

-- Binary arithmetic operators

add :: Expression -> Expression -> Expression
add lhs rhs = BinaryArithOp $ BinaryArithOp' Add lhs rhs

subtract :: Expression -> Expression -> Expression
subtract lhs rhs = BinaryArithOp $ BinaryArithOp' Subtract lhs rhs

multiply :: Expression -> Expression -> Expression
multiply lhs rhs = BinaryArithOp $ BinaryArithOp' Multiply lhs rhs

divide :: Expression -> Expression -> Expression
divide lhs rhs = BinaryArithOp $ BinaryArithOp' Divide lhs rhs

-- Binary logical operators

and :: Expression -> Expression -> Expression
and lhs rhs = BinaryLogicalOp $ BinaryLogicalOp' And lhs rhs

or :: Expression -> Expression -> Expression
or lhs rhs = BinaryLogicalOp $ BinaryLogicalOp' Or lhs rhs

-- Comparison operators

lessThan :: Expression -> Expression -> Expression
lessThan lhs rhs = Comparison $ Comparison' LessThan lhs rhs

lessThanOrEquals :: Expression -> Expression -> Expression
lessThanOrEquals lhs rhs = Comparison $ Comparison' LessThanOrEquals lhs rhs

equals :: Expression -> Expression -> Expression
equals lhs rhs = Comparison $ Comparison' Equals lhs rhs

greaterThanOrEquals :: Expression -> Expression -> Expression
greaterThanOrEquals lhs rhs =
  Comparison $ Comparison' GreaterThanOrEquals lhs rhs

greaterThan :: Expression -> Expression -> Expression
greaterThan lhs rhs = Comparison $ Comparison' GreaterThan lhs rhs

notEquals :: Expression -> Expression -> Expression
notEquals lhs rhs = Comparison $ Comparison' NotEquals lhs rhs
