module Elements.Compiler.Fragment
  ( Fragment
  , append
  , singleton
  , toList
  , length
  )
where

import           Prelude                 hiding ( length )
import           Data.DList                     ( DList )
import qualified Data.DList                    as DList

data Fragment a = Fragment
  { fragmentContents :: DList a
  , fragmentLength :: Int
  }
  deriving stock (Show, Eq)

instance Semigroup (Fragment a) where
  (Fragment lfb lfs) <> (Fragment rfb rfs) = Fragment (lfb <> rfb) (lfs + rfs)

instance Monoid (Fragment a) where
  mempty = Fragment DList.empty 0

singleton :: a -> Fragment a
singleton a = Fragment (DList.singleton a) 1

append :: a -> Fragment a -> Fragment a
append a (Fragment fb fs) = Fragment (DList.snoc fb a) (fs + 1)

toList :: Fragment a -> [a]
toList = DList.toList . fragmentContents

length :: Fragment a -> Int
length = fragmentLength
