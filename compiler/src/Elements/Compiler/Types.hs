module Elements.Compiler.Types where

import           Data.Aeson                     ( ToJSON )
import           Elements.Bytecode              ( Bytecode )
import           GHC.Generics                   ( Generic )

newtype Program = Program
  { bytecodes :: [Bytecode]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)
