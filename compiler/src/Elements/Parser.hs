module Elements.Parser
  ( Parser
  , TextParseError
  , pExpression
  ) where

import           Control.Monad.Combinators.Expr ( Operator(..)
                                                , makeExprParser
                                                )
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Void                      ( Void )
import           Text.Megaparsec         hiding ( State )
import qualified Text.Megaparsec.Char          as MC
import qualified Text.Megaparsec.Char.Lexer    as MCL

import           Elements.AST                   ( Expression(..) )
import qualified Elements.AST                  as AST

type Parser = Parsec Void Text

type TextParseError = ParseErrorBundle Text Void

spaceConsumer :: Parser ()
spaceConsumer = MCL.space MC.space1 empty empty

lexeme :: Parser a -> Parser a
lexeme = MCL.lexeme spaceConsumer

symbol :: Text -> Parser Text
symbol = MCL.symbol spaceConsumer

pInteger :: Parser Expression
pInteger = NumericLiteral . AST.IntValue <$> lexeme MCL.decimal

pBool :: Parser Expression
pBool = lexeme $ trueParser <|> falseParser
 where
  trueParser  = BoolLiteral True <$ MC.string "true"
  falseParser = BoolLiteral False <$ MC.string "false"

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (MC.string keyword <* notFollowedBy MC.alphaNumChar)

operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ [prefix "-" Negate, prefix "+" id]
  , [binary "+" AST.add, binary "-" AST.subtract]
  , [ binary "==" AST.equals
    , binary "!=" AST.notEquals
    , binary "<=" AST.lessThanOrEquals
    , binary "<"  AST.lessThan
    , binary ">=" AST.greaterThanOrEquals
    , binary ">"  AST.greaterThan
    ]
  ]

pTerm :: Parser Expression
pTerm = choice [pInteger, pBool]

pExpression :: Parser Expression
pExpression = makeExprParser pTerm operatorTable <|> pIfElse

pIfElse :: Parser Expression
pIfElse = do
  pKeyword "if"
  testCondition <- pExpression
  pKeyword "then"
  thenExpr <- pExpression
  pKeyword "else"
  elseExpr <- pExpression
  return $ IfElse $ AST.IfElse' testCondition thenExpr elseExpr

binary
  :: Text
  -> (Expression -> Expression -> Expression)
  -> Operator Parser Expression
binary name f = InfixL (f <$ symbol name)

prefix :: Text -> (Expression -> Expression) -> Operator Parser Expression
prefix name f = Prefix (f <$ symbol name)
