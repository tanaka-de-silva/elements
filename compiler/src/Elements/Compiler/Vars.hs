module Elements.Compiler.Vars
  ( VarInfo(..)
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
import           Elements.Compiler.Types        ( DataType
                                                , LocalVarIndex(..)
                                                , dataTypeByteCount
                                                )

data VarInfo = VarInfo
  { localVarIndex :: LocalVarIndex
  , lineNum       :: LineNumber
  , dataType      :: DataType
  }

data Vars = Vars
  { identifiers :: HashMap Identifier VarInfo
  , nextIndex :: Int32
  }

empty :: Vars
empty = Vars { identifiers = HashMap.empty, nextIndex = 0 }

addVar :: Identifier -> LineNumber -> DataType -> Vars -> (LocalVarIndex, Vars)
addVar identifier lNum dType (Vars currentIdentifiers n) =
  let varIndex       = LocalVarIndex n
      varInfo        = VarInfo varIndex lNum dType
      newIdentifiers = HashMap.insert identifier varInfo currentIdentifiers
      size           = fromIntegral $ dataTypeByteCount dType
  in  (varIndex, Vars newIdentifiers (n + size))

lookupVar :: Identifier -> Vars -> Maybe VarInfo
lookupVar x = HashMap.lookup x . identifiers
