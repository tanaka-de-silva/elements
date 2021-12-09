module Elements.Bytecode where

import           Data.Aeson                     ( ToJSON )
import           GHC.Generics                   ( Generic )
import           GHC.Int                        ( Int32 )

data Bytecode = PushInt Int32
              | Add
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)
