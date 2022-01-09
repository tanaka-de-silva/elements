module Elements.Compiler.Types where

import           Data.Aeson                     ( ToJSON )
import           Data.HashMap.Strict            ( HashMap )
import           GHC.Generics                   ( Generic )
import           GHC.Int                        ( Int32 )

import qualified Elements.AST                  as AST

newtype LocalVarIndex = LocalVarIndex Int32
  deriving newtype (Eq, Show, Num, ToJSON)

data NumericType = IntType
                 | LongType
                 | DoubleType
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data DataType = NumericType NumericType
              | BoolType
  deriving (Show, Eq)

showDataType :: DataType -> String
showDataType = \case
  BoolType -> "Bool"
  NumericType IntType -> "Int"
  NumericType LongType -> "Long"
  NumericType DoubleType -> "Double"

dataTypeByteCount :: DataType -> Int
dataTypeByteCount = \case
  BoolType -> 4 -- represented as an int
  NumericType IntType -> 4
  NumericType LongType -> 8
  NumericType DoubleType -> 8

data DuplicateValueDefinitionError' = DuplicateValueDefinitionError'
  { duplicateVal :: AST.Identifier
  , originalLine :: AST.LineNumber
  , duplicateLine :: AST.LineNumber
  } deriving (Show, Eq)

data TypeError' = TypeError'
  { expectedType :: DataType
  , actualType   :: DataType
  }
  deriving (Show, Eq)

data CompileError = DuplicateValueDefinitionError DuplicateValueDefinitionError'
                  | UndefinedValueError AST.Identifier AST.LineNumber
                  | UndefinedOperatorError
                  | TypeError TypeError'
                    deriving (Show, Eq)

data VarTypeInfo = VarTypeInfo
  { vtDataType    :: DataType
  , vtLineNum     :: AST.LineNumber
  }

type VarTypes = HashMap AST.Identifier VarTypeInfo
