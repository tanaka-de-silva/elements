module Elements.Compiler.Types where

import           Data.Aeson                     ( ToJSON )
import           GHC.Generics                   ( Generic )
import           Elements.Bytecode              ( Bytecode )

newtype Program = Program
  { bytecodes :: [Bytecode]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)
