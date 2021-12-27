module Elements.Compiler.Types where

import           Data.Aeson                     ( ToJSON )
import           Data.HashMap.Strict            ( HashMap )
import           GHC.Generics                   ( Generic )

import qualified Elements.AST                  as AST
import           Elements.Bytecode              ( Bytecode )

newtype Program = Program
  { bytecodes :: [Bytecode]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data DataType = BoolType
              | IntType
  deriving (Show, Eq)

showDataType :: DataType -> String
showDataType = \case
  BoolType -> "Bool"
  IntType -> "Int"

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
                  | TypeError TypeError'
                    deriving (Show, Eq)

data VarTypeInfo = VarTypeInfo
  { vtDataType    :: DataType
  , vtLineNum     :: AST.LineNumber
  }

type VarTypes = HashMap AST.Identifier VarTypeInfo
