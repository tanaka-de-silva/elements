module Elements.ParserSpec
  ( spec
  ) where

import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import qualified Elements.AST                  as AST
import           Elements.Parser                ( TextParseError
                                                , pExpression
                                                )
import           Elements.Syntax                ( int )
import           Test.Hspec
import qualified Text.Megaparsec               as Megaparsec

parseExpr :: Text -> Either TextParseError AST.Expression
parseExpr = Megaparsec.runParser pExpression "ParserSpec.hs"

parseIntegers :: IO ()
parseIntegers = parseExpr "1" `shouldBe` Right (int 1)

parseBooleans :: IO ()
parseBooleans = do
  parseExpr "true" `shouldBe` Right (AST.BoolLiteral True)
  parseExpr "false" `shouldBe` Right (AST.BoolLiteral False)

parseBinaryArithmetic :: IO ()
parseBinaryArithmetic = do
  parseExpr "1 + 2" `shouldBe` Right (AST.add (int 1) (int 2))
  parseExpr "3 - 1" `shouldBe` Right (AST.subtract (int 3) (int 1))

parseComparisons :: IO ()
parseComparisons = do
  parseExpr "1 <  2" `shouldBe` Right (AST.lessThan (int 1) (int 2))
  parseExpr "1 <= 2" `shouldBe` Right (AST.lessThanOrEquals (int 1) (int 2))
  parseExpr "1 == 1" `shouldBe` Right (AST.equals (int 1) (int 1))
  parseExpr "3 >= 2" `shouldBe` Right (AST.greaterThanOrEquals (int 3) (int 2))
  parseExpr "3  > 2" `shouldBe` Right (AST.greaterThan (int 3) (int 2))
  parseExpr "3 != 2" `shouldBe` Right (AST.notEquals (int 3) (int 2))

parseNegation :: IO ()
parseNegation = parseExpr "-1" `shouldBe` Right (AST.Negate (int 1))

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

spec :: Spec
spec = do
  it "can parse integers"                       parseIntegers
  it "can parse booleans"                       parseBooleans
  it "can parses binary arithmetic expressions" parseBinaryArithmetic
  it "can parses comparison expressions"        parseComparisons
  it "can parse a negation expression"          parseNegation
  it "can parse a poitive prefix expression"    parsePostivePrefix
  it "can parse an if else expression"          parseIfElse
