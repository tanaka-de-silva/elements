module Elements.AST where

import           Data.Aeson                     ( ToJSON )
import           GHC.Generics                   ( Generic )
import           GHC.Int                        ( Int32 )

newtype NumericValue = IntValue Int32
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data ArithmeticOp = Add
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data BinaryOp' = BinaryOp'
  { binOp :: ArithmeticOp
  , binOpLhs :: Expression
  , binOpRhs :: Expression
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data Expression = NumericLiteral NumericValue
                | BinaryOp BinaryOp'
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

add :: Expression -> Expression -> Expression
add lhs rhs = BinaryOp $ BinaryOp' Add lhs rhs
