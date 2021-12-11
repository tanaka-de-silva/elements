module Elements.CompilerSpec
  ( spec
  )
where

import           Data.Text                      ( Text )
import qualified Elements.AST                  as AST
import qualified Elements.Bytecode             as Bytecode
import           Elements.Bytecode              ( Bytecode )
import           Elements.Compiler              ( compileExpression )
import qualified Elements.Compiler.Fragment    as Fragment
import           Elements.Compiler.Fragment     ( Fragment )
import           Elements.Syntax                ( int )
import           Test.Hspec

testExpression :: AST.Expression -> [Bytecode]
testExpression = Fragment.toList . compileExpression

pushIntegersValues :: IO ()
pushIntegersValues =
  let program  = int 1
      result   = testExpression program
      expected = [Bytecode.PushInt 1]
  in  result `shouldBe` expected

addition :: IO ()
addition =
  let program  = AST.add (int 1) (int 2)
      result   = testExpression program
      expected = [Bytecode.PushInt 1, Bytecode.PushInt 2, Bytecode.Add]
  in  result `shouldBe` expected

subtraction :: IO ()
subtraction =
  let program  = AST.subtract (int 3) (int 1)
      result   = testExpression program
      expected = [Bytecode.PushInt 3, Bytecode.PushInt 1, Bytecode.Subtract]
  in  result `shouldBe` expected

spec :: Spec
spec = do
  it "can push integer values on to the stack" pushIntegersValues
  it "can add two numbers together"            addition
  it "can subtract one number from another"    subtraction
