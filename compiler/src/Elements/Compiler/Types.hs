module Elements.Compiler.Types where

import           Control.Monad.Except           ( Except
                                                , MonadError
                                                )
import qualified Control.Monad.Except          as Except
import           Control.Monad.State.Strict     ( MonadState
                                                , StateT
                                                )
import           Data.Aeson                     ( ToJSON )
import qualified Elements.AST                  as AST
import           Elements.Bytecode              ( Bytecode )
import           GHC.Generics                   ( Generic )

newtype Program = Program
  { bytecodes :: [Bytecode]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

data DuplicateValueDefinitionError' = DuplicateValueDefinitionError' 
  { duplicateVal :: AST.Identifier 
  , originalLine :: Int
  , duplicateLine :: Int
  } deriving (Show, Eq)

data CompileError = DuplicateValueDefinitionError DuplicateValueDefinitionError'
                  | UndefinedValueError AST.Identifier
                    deriving (Show, Eq)

newtype CompilerM a = CompilerM { unwrapCompilerM :: Except CompileError a }
  deriving newtype(
    Functor, Applicative, Monad,
    MonadError CompileError
  )

runCompilerM :: CompilerM a -> Either CompileError a
runCompilerM = Except.runExcept . unwrapCompilerM 
