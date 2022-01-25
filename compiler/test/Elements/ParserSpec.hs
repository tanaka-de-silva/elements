{-# LANGUAGE QuasiQuotes #-}

module Elements.ParserSpec
  ( spec
  ) where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Elements.AST                  as AST
import           Elements.Parser                ( TextParseError
                                                , pExpression
                                                )
import           Elements.Syntax                ( double
                                                , int
                                                , long
                                                )
import           Test.Hspec
import           Text.InterpolatedString.Perl6  ( q )
import qualified Text.Megaparsec               as Megaparsec

parseExpr :: Text -> Either TextParseError AST.Expression
parseExpr = Megaparsec.runParser pExpression "ParserSpec.hs"

parseIntegers :: IO ()
parseIntegers = parseExpr "1" `shouldBe` Right (int 1)

parseLongs :: IO ()
parseLongs = parseExpr "1L" `shouldBe` Right (long 1)

parseDoubles :: IO ()
parseDoubles = parseExpr "1.0" `shouldBe` Right (double 1)

parseBooleans :: IO ()
parseBooleans = do
  parseExpr "true" `shouldBe` Right (AST.BoolLiteral True)
  parseExpr "false" `shouldBe` Right (AST.BoolLiteral False)

parseBinaryArithmetic :: IO ()
parseBinaryArithmetic = do
  parseExpr "1 + 2" `shouldBe` Right (AST.add (int 1) (int 2))
  parseExpr "3 - 1" `shouldBe` Right (AST.subtract (int 3) (int 1))
  parseExpr "2 * 3" `shouldBe` Right (AST.multiply (int 2) (int 3))
  parseExpr "4 / 2" `shouldBe` Right (AST.divide (int 4) (int 2))

parseBinaryLogical :: IO ()
parseBinaryLogical = do
  parseExpr "true || false"
    `shouldBe` Right (AST.or (AST.BoolLiteral True) (AST.BoolLiteral False))
  parseExpr "true && true"
    `shouldBe` Right (AST.and (AST.BoolLiteral True) (AST.BoolLiteral True))

parseComparisons :: IO ()
parseComparisons = do
  parseExpr "1 <  2" `shouldBe` Right (AST.lessThan (int 1) (int 2))
  parseExpr "1 <= 2" `shouldBe` Right (AST.lessThanOrEquals (int 1) (int 2))
  parseExpr "1 == 1" `shouldBe` Right (AST.equals (int 1) (int 1))
  parseExpr "3 >= 2" `shouldBe` Right (AST.greaterThanOrEquals (int 3) (int 2))
  parseExpr "3  > 2" `shouldBe` Right (AST.greaterThan (int 3) (int 2))
  parseExpr "3 != 2" `shouldBe` Right (AST.notEquals (int 3) (int 2))

parseNegation :: IO ()
parseNegation = parseExpr "-1" `shouldBe` Right (AST.UnaryMinus (int 1))

parsePostivePrefix :: IO ()
parsePostivePrefix = parseExpr "+1" `shouldBe` Right (int 1)

parseIfElse :: IO ()
parseIfElse =
  let result   = parseExpr "if true then 1 else 0"
      expected = AST.IfElse $ AST.IfElse'
        { AST.testCondition = AST.BoolLiteral True
        , AST.thenExpr      = int 1
        , AST.elseExpr      = int 0
        }
  in  result `shouldBe` Right expected

parseSingleValBinding :: IO ()
parseSingleValBinding =
  let result = parseExpr $ Text.strip [q|
        val x = 1
        x + 1
      |]
      expected = AST.ValBinding $ AST.ValBinding'
        { AST.vbIdentifier = "x"
        , AST.vbLineNum    = 1
        , AST.vbBoundExpr  = int 1
        , AST.vbBaseExpr   = AST.add (AST.value "x" 2) (int 1)
        }
  in  result `shouldBe` Right expected

parseMultipleValBindings :: IO ()
parseMultipleValBindings =
  let result = parseExpr $ Text.strip [q| 
        val x = 1
        val y = 2
        x + y
      |]
      innerBinding = AST.ValBinding $ AST.ValBinding'
        { AST.vbIdentifier = "y"
        , AST.vbLineNum    = 2
        , AST.vbBoundExpr  = int 2
        , AST.vbBaseExpr   = AST.add (AST.value "x" 3) (AST.value "y" 3)
        }
      expected = AST.ValBinding $ AST.ValBinding'
        { AST.vbIdentifier = "x"
        , AST.vbLineNum    = 1
        , AST.vbBoundExpr  = int 1
        , AST.vbBaseExpr   = innerBinding
        }
  in  result `shouldBe` Right expected

spec :: Spec
spec = do
  it "can parse integers"                       parseIntegers
  it "can parse longs"                          parseLongs
  it "can parse doubles"                        parseDoubles
  it "can parse booleans"                       parseBooleans
  it "can parses binary arithmetic expressions" parseBinaryArithmetic
  it "can parses binary logical expressions"    parseBinaryLogical
  it "can parses comparison expressions"        parseComparisons
  it "can parse a negation expression"          parseNegation
  it "can parse a poitive prefix expression"    parsePostivePrefix
  it "can parse an if else expression"          parseIfElse
  it "can parse a single val binding"           parseSingleValBinding
  it "can parse multiple val bindings"          parseMultipleValBindings
