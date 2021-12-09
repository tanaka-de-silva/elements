module Elements.Compiler.FragmentSpec
  ( spec
  )
where

import qualified Elements.Compiler.Fragment    as Fragment
import           Elements.Compiler.Fragment     ( Fragment )
import           Test.Hspec

createFragment :: IO ()
createFragment = do
  let fragment = Fragment.singleton 1
  Fragment.length fragment `shouldBe` 1
  Fragment.toList fragment `shouldBe` [1]

appendToFragment :: IO ()
appendToFragment = do
  let fragment = Fragment.append 2 $ Fragment.singleton 1
  Fragment.length fragment `shouldBe` 2
  Fragment.toList fragment `shouldBe` [1, 2]

spec :: Spec
spec = do
  it "can create a fragment"    createFragment
  it "can append to a fragment" appendToFragment
