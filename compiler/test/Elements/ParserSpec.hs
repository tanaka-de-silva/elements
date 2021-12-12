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

parseBinaryArithmetic :: IO ()
parseBinaryArithmetic = do
  parseExpr "1 + 2" `shouldBe` Right (AST.add (int 1) (int 2))
  parseExpr "3 - 1" `shouldBe` Right (AST.subtract (int 3) (int 1))

parseNegation :: IO ()
parseNegation = parseExpr "-1" `shouldBe` Right (AST.Negate (int 1))

parsePostivePrefix :: IO ()
parsePostivePrefix = parseExpr "+1" `shouldBe` Right (int 1)

spec :: Spec
spec = do
  it "can parse integers"                       parseIntegers
  it "can parses binary arithmetic expressions" parseBinaryArithmetic
  it "can parse a negation expression"          parseNegation
  it "can parse a poitive prefix expression"    parsePostivePrefix
