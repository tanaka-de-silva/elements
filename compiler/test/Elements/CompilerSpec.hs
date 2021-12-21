module Elements.CompilerSpec
  ( spec
  ) where

import           Data.Text                      ( Text )
import           Test.Hspec

import qualified Elements.AST                  as AST
import qualified Elements.Bytecode             as Bytecode
import           Elements.Bytecode              ( Bytecode )
import           Elements.Compiler              ( compileExpression )
import qualified Elements.Compiler.Fragment    as Fragment
import           Elements.Compiler.Fragment     ( Fragment )
import           Elements.Compiler.Types        ( CompileError
                                                , runCompilerM
                                                )
import qualified Elements.Compiler.Types       as CompilerT
import qualified Elements.Compiler.Vars        as Vars
import           Elements.Syntax                ( int )

testExpression :: AST.Expression -> Either CompileError [Bytecode]
testExpression =
  fmap Fragment.toList . runCompilerM . compileExpression Vars.empty

pushIntegerValue :: IO ()
pushIntegerValue =
  let program  = int 1
      result   = testExpression program
      expected = [Bytecode.PushInt 1]
  in  result `shouldBe` Right expected

pushBooleanValue :: IO ()
pushBooleanValue = do
  testExpression (AST.BoolLiteral False) `shouldBe` Right [Bytecode.PushInt 0]
  testExpression (AST.BoolLiteral True) `shouldBe` Right [Bytecode.PushInt 1]

addition :: IO ()
addition =
  let program  = AST.add (int 1) (int 2)
      result   = testExpression program
      expected = [Bytecode.PushInt 1, Bytecode.PushInt 2, Bytecode.Add]
  in  result `shouldBe` Right expected

subtraction :: IO ()
subtraction =
  let program  = AST.subtract (int 3) (int 1)
      result   = testExpression program
      expected = [Bytecode.PushInt 3, Bytecode.PushInt 1, Bytecode.Subtract]
  in  result `shouldBe` Right expected

multiplication :: IO ()
multiplication =
  let program  = AST.multiply (int 2) (int 3)
      result   = testExpression program
      expected = [Bytecode.PushInt 2, Bytecode.PushInt 3, Bytecode.Multiply]
  in  result `shouldBe` Right expected

division :: IO ()
division =
  let program  = AST.divide (int 4) (int 2)
      result   = testExpression program
      expected = [Bytecode.PushInt 4, Bytecode.PushInt 2, Bytecode.Divide]
  in  result `shouldBe` Right expected

negation :: IO ()
negation =
  let program  = AST.Negate (int 1)
      result   = testExpression program
      expected = [Bytecode.PushInt 1, Bytecode.Negate]
  in  result `shouldBe` Right expected

checkEquality :: IO ()
checkEquality =
  let program  = AST.equals (int 1) (int 1)
      result   = testExpression program
      expected = [Bytecode.PushInt 1, Bytecode.PushInt 1, Bytecode.Equals]
  in  result `shouldBe` Right expected

checkLessThan :: IO ()
checkLessThan =
  let program  = AST.lessThan (int 1) (int 2)
      result   = testExpression program
      expected = [Bytecode.PushInt 1, Bytecode.PushInt 2, Bytecode.LessThan]
  in  result `shouldBe` Right expected

checkLessThanOrEqual :: IO ()
checkLessThanOrEqual =
  let
    program = AST.lessThanOrEquals (int 1) (int 2)
    result  = testExpression program
    expected =
      [Bytecode.PushInt 1, Bytecode.PushInt 2, Bytecode.LessThanOrEquals]
  in
    result `shouldBe` Right expected

checkGreaterThan :: IO ()
checkGreaterThan =
  let
    program  = AST.greaterThan (int 2) (int 1)
    result   = testExpression program
    expected = [Bytecode.PushInt 2, Bytecode.PushInt 1, Bytecode.GreaterThan]
  in
    result `shouldBe` Right expected

checkGreaterOrEqual :: IO ()
checkGreaterOrEqual =
  let program = AST.greaterThanOrEquals (int 2) (int 1)
      result  = testExpression program
      expected =
        [Bytecode.PushInt 2, Bytecode.PushInt 1, Bytecode.GreaterThanOrEquals]
  in  result `shouldBe` Right expected

checkInequality :: IO ()
checkInequality =
  let program  = AST.notEquals (int 1) (int 2)
      result   = testExpression program
      expected = [Bytecode.PushInt 1, Bytecode.PushInt 2, Bytecode.NotEquals]
  in  result `shouldBe` Right expected

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
  in  result `shouldBe` Right expected

simpleValBinding :: IO ()
simpleValBinding =
  let
    program = AST.ValBinding $ AST.ValBinding' { AST.identifier = "x"
                                               , AST.lineNum    = 1
                                               , AST.boundExpr  = int 1
                                               , AST.baseExpr   = AST.Value "x"
                                               }
    result   = testExpression program
    expected = [Bytecode.PushInt 1, Bytecode.StoreLocal 0, Bytecode.GetLocal 0]
  in
    result `shouldBe` Right expected

undefinedValueRef :: IO ()
undefinedValueRef =
  let program  = AST.Value "x"
      result   = testExpression program
      expected = CompilerT.UndefinedValueError "x"
  in  result `shouldBe` Left expected

multipleValBindings :: IO ()
multipleValBindings =
  let innerBinding = AST.ValBinding $ AST.ValBinding'
        { AST.identifier = "y"
        , AST.lineNum    = 2
        , AST.boundExpr  = int 2
        , AST.baseExpr   = AST.add (AST.Value "x") (AST.Value "y")
        }

      program = AST.ValBinding $ AST.ValBinding' { AST.identifier = "x"
                                                 , AST.lineNum    = 1
                                                 , AST.boundExpr  = int 1
                                                 , AST.baseExpr   = innerBinding
                                                 }

      result = testExpression program
      expected =
        [ Bytecode.PushInt 1
        , Bytecode.StoreLocal 0
        , Bytecode.PushInt 2
        , Bytecode.StoreLocal 1
        , Bytecode.GetLocal 0
        , Bytecode.GetLocal 1
        , Bytecode.Add
        ]
  in  result `shouldBe` Right expected

duplicateValueDefinition :: IO ()
duplicateValueDefinition =
  let innerBinding = AST.ValBinding $ AST.ValBinding'
        { AST.identifier = "x"
        , AST.lineNum    = 2
        , AST.boundExpr  = int 2
        , AST.baseExpr   = AST.Value "x"
        }

      program = AST.ValBinding $ AST.ValBinding' { AST.identifier = "x"
                                                 , AST.lineNum    = 1
                                                 , AST.boundExpr  = int 1
                                                 , AST.baseExpr   = innerBinding
                                                 }
      result = testExpression program
      expected =
        CompilerT.DuplicateValueDefinitionError
          $ CompilerT.DuplicateValueDefinitionError'
              { CompilerT.duplicateVal  = "x"
              , CompilerT.originalLine  = 1
              , CompilerT.duplicateLine = 2
              }
  in  result `shouldBe` Left expected

spec :: Spec
spec = do
  it "can push an integer value on to the stack" pushIntegerValue
  it "can push a boolean value on to the stack"  pushBooleanValue
  it "can add two numbers together"              addition
  it "can subtract one number from another"      subtraction
  it "can multiply a number by another"          multiplication
  it "can divide a number by another"            division
  it "can negate a number"                       negation
  it "can check equality"                        checkEquality
  it "can check if a value is less than another" checkLessThan
  it "can check if a value is less than or equal to another"
     checkLessThanOrEqual
  it "can check if a value is greater than another" checkGreaterThan
  it "can check if a value is greater or than equal to another"
     checkGreaterOrEqual
  it "can check inequality"                       checkInequality
  it "can handle simple if else expressions"      simpleIfElse
  it "can handle a simple val binding"            simpleValBinding
  it "returns an error when an undefined value is referenced" undefinedValueRef
  it "can handle multiple val bindings"           multipleValBindings
  it "returns an error when a value is redefined" duplicateValueDefinition
