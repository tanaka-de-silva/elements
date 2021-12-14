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
  testExpression (AST.BoolLiteral False) `shouldBe` [Bytecode.PushInt 0]
  testExpression (AST.BoolLiteral True) `shouldBe` [Bytecode.PushInt 1]

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

checkEquality :: IO ()
checkEquality =
  let program  = AST.equals (int 1) (int 1)
      result   = testExpression program
      expected = [Bytecode.PushInt 1, Bytecode.PushInt 1, Bytecode.Equals]
  in  result `shouldBe` expected

checkLessThan :: IO ()
checkLessThan =
  let program  = AST.lessThan (int 1) (int 2)
      result   = testExpression program
      expected = [Bytecode.PushInt 1, Bytecode.PushInt 2, Bytecode.LessThan]
  in  result `shouldBe` expected

checkLessThanOrEqual :: IO ()
checkLessThanOrEqual =
  let
    program = AST.lessThanOrEquals (int 1) (int 2)
    result  = testExpression program
    expected =
      [Bytecode.PushInt 1, Bytecode.PushInt 2, Bytecode.LessThanOrEquals]
  in
    result `shouldBe` expected

checkGreaterThan :: IO ()
checkGreaterThan =
  let
    program  = AST.greaterThan (int 2) (int 1)
    result   = testExpression program
    expected = [Bytecode.PushInt 2, Bytecode.PushInt 1, Bytecode.GreaterThan]
  in
    result `shouldBe` expected

checkGreaterOrEqual :: IO ()
checkGreaterOrEqual =
  let program = AST.greaterThanOrEquals (int 2) (int 1)
      result  = testExpression program
      expected =
        [Bytecode.PushInt 2, Bytecode.PushInt 1, Bytecode.GreaterThanOrEquals]
  in  result `shouldBe` expected

checkInequality :: IO ()
checkInequality =
  let program  = AST.notEquals (int 1) (int 2)
      result   = testExpression program
      expected = [Bytecode.PushInt 1, Bytecode.PushInt 2, Bytecode.NotEquals]
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
  it "can check equality"                        checkEquality
  it "can check if a value is less than another" checkLessThan
  it "can check if a value is less than or equal to another"
     checkLessThanOrEqual
  it "can check if a value is greater than another" checkGreaterThan
  it "can check if a value is greater or than equal to another"
     checkGreaterOrEqual
  it "can check inequality"                  checkInequality
  it "can handle simple if else expressions" simpleIfElse
