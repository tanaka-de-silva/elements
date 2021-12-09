module Main where

import           Elements.Compiler              ( compile )
import qualified System.Environment            as System

main :: IO ()
main = do
  sourcePath <- head <$> System.getArgs
  compile sourcePath
