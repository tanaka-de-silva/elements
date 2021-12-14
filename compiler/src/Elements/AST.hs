module Elements.AST where

import           Data.Aeson                     ( ToJSON )
import           GHC.Generics                   ( Generic )
import           GHC.Int                        ( Int32 )

newtype NumericValue = IntValue Int32
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data ArithmeticOp = Add
                  | Subtract
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data BinaryOp' = BinaryOp'
  { binOp    :: ArithmeticOp
  , binOpLhs :: Expression
  , binOpRhs :: Expression
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

data Expression = NumericLiteral NumericValue
                | BoolLiteral Bool
                | Negate Expression
                | BinaryOp BinaryOp'
                | Comparison Comparison'
                | IfElse IfElse'
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

add :: Expression -> Expression -> Expression
add lhs rhs = BinaryOp $ BinaryOp' Add lhs rhs

subtract :: Expression -> Expression -> Expression
subtract lhs rhs = BinaryOp $ BinaryOp' Subtract lhs rhs

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
