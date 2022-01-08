module Elements.Bytecode where

import           Data.Aeson                     ( ToJSON )
import           Elements.Compiler.Types        ( LocalVarIndex
                                                , NumericType
                                                )
import           GHC.Generics                   ( Generic )
import           GHC.Int                        ( Int32
                                                , Int64
                                                )

newtype PCOffset = PCOffset Int32
  deriving newtype (Eq, Show, Num, ToJSON)

data Bytecode = PushInt Int32
              | PushLong Int64
              | PushDouble Double
              | Add NumericType
              | Subtract NumericType
              | Multiply NumericType
              | Divide NumericType
              | Negate NumericType
              | LessThan NumericType
              | LessThanOrEquals NumericType
              | Equals NumericType
              | GreaterThanOrEquals NumericType
              | GreaterThan NumericType
              | NotEquals NumericType
              | And
              | Or
              | BranchIfFalse PCOffset
              | Goto PCOffset
              | StoreLocalInt LocalVarIndex
              | StoreLocalLong LocalVarIndex
              | StoreLocalDouble LocalVarIndex
              | GetLocalInt LocalVarIndex
              | GetLocalLong LocalVarIndex
              | GetLocalDouble LocalVarIndex
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

newtype Program = Program
  { bytecodes :: [Bytecode]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)
