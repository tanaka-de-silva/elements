module Elements.Compiler.Vars
  ( LocalVarIndex(..)
  , VarInfo(..)
  , Vars
  , empty
  , addVar
  , lookupVar
  )
where

import           Data.Aeson                     ( ToJSON )
import           Data.HashMap.Strict            ( HashMap )
import qualified Data.HashMap.Strict           as HashMap
import           GHC.Generics                   ( Generic )
import           GHC.Int                        ( Int32 )

import           Elements.AST                   ( Identifier(..)
                                                , LineNumber
                                                )

newtype LocalVarIndex = LocalVarIndex Int32
  deriving newtype (Eq, Show, Num, ToJSON)

data VarInfo = VarInfo
  { localVarIndex :: LocalVarIndex
  , lineNum       :: LineNumber
  }

data Vars = Vars
  { identifiers :: HashMap Identifier VarInfo
  , nextIndex :: Int32
  }

empty :: Vars
empty = Vars { identifiers = HashMap.empty, nextIndex = 0 }

addVar :: Identifier -> LineNumber -> Vars -> (LocalVarIndex, Vars)
addVar identifier lNum (Vars currentIdentifiers n) =
  let varIndex       = LocalVarIndex n
      varInfo        = VarInfo varIndex lNum
      newIdentifiers = HashMap.insert identifier varInfo currentIdentifiers
  in  (varIndex, Vars newIdentifiers (n + 1))

lookupVar :: Identifier -> Vars -> Maybe VarInfo
lookupVar x = HashMap.lookup x . identifiers
