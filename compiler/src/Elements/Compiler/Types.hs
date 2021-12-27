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

data DuplicateValueDefinitionError' = DuplicateValueDefinitionError'
  { duplicateVal :: AST.Identifier
  , originalLine :: Int
  , duplicateLine :: Int
  } deriving (Show, Eq)

data TypeError' = TypeError'
  { expectedType :: DataType
  , actualType   :: DataType
  }
  deriving (Show, Eq)

data CompileError = DuplicateValueDefinitionError DuplicateValueDefinitionError'
                  | UndefinedValueError AST.Identifier
                  | TypeError TypeError'
                    deriving (Show, Eq)

data VarTypeInfo = VarTypeInfo
  { vtDataType    :: DataType
  , vtLineNum     :: Int
  }

type VarTypes = HashMap AST.Identifier VarTypeInfo
