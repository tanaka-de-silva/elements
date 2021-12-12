module Elements.CompilerSpec
  ( spec
  ) where

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

pushIntegerValue :: IO ()
pushIntegerValue =
  let program  = int 1
      result   = testExpression program
      expected = [Bytecode.PushInt 1]
  in  result `shouldBe` expected

pushBooleanValue :: IO ()
pushBooleanValue = do
  testExpression (AST.BoolLiteral False) `shouldBe`  [Bytecode.PushInt 0]
  testExpression (AST.BoolLiteral True) `shouldBe`  [Bytecode.PushInt 1]

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

negation :: IO ()
negation =
  let program  = AST.Negate (int 1)
      result   = testExpression program
      expected = [Bytecode.PushInt 1, Bytecode.Negate]
  in  result `shouldBe` expected

simpleIfElse :: IO ()
simpleIfElse =
  let program = AST.IfElse $ AST.IfElse'
        { AST.testCondition = AST.BoolLiteral True
        , AST.thenExpr      = int 1
        , AST.elseExpr      = int 0
        }
      result = testExpression program
      expected =
        [ Bytecode.PushInt 1
        , Bytecode.BranchIfFalse 2
        , Bytecode.PushInt 1
        , Bytecode.Goto 1
        , Bytecode.PushInt 0
        ]
  in  result `shouldBe` expected

spec :: Spec
spec = do
  it "can push an integer value on to the stack" pushIntegerValue
  it "can push a boolean value on to the stack"  pushBooleanValue
  it "can add two numbers together"              addition
  it "can subtract one number from another"      subtraction
  it "can negate a number"                       negation
  it "can handle simple if else expressions"     simpleIfElse
