module Elements.Bytecode where

import           Data.Aeson                     ( ToJSON )
import           GHC.Generics                   ( Generic )
import           GHC.Int                        ( Int32 )

newtype PCOffset = PCOffset Int32
  deriving newtype (Eq, Show, Num, ToJSON)

data Bytecode = PushInt Int32
              | Add
              | Subtract
              | Negate
              | LessThan
              | LessThanOrEquals
              | Equals
              | GreaterThanOrEquals
              | GreaterThan
              | NotEquals
              | BranchIfFalse PCOffset
              | Goto PCOffset
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)
