module Elements.Parser
  ( Parser
  , TextParseError
  , pExpression
  ) where

import           Control.Monad.Combinators.Expr ( Operator(..)
                                                , makeExprParser
                                                )
import qualified Data.HashSet                  as HashSet
import           Data.Text                      ( Text )
import qualified Data.Text                     as Text
import           Data.Void                      ( Void )
import           Elements.AST                   ( Expression(..) )
import qualified Elements.AST                  as AST
import           Text.Megaparsec         hiding ( State )
import qualified Text.Megaparsec.Char          as MC
import qualified Text.Megaparsec.Char.Lexer    as MCL
import qualified Text.Megaparsec.Pos           as MP

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

reservedKeywords :: HashSet.HashSet Text
reservedKeywords =
  HashSet.fromList $ boolKeywords ++ condKeywords ++ defKeywords
 where
  boolKeywords = ["true", "false"]
  condKeywords = ["if", "then", "else"]
  defKeywords  = ["val"]

pIdentifier :: Parser AST.Identifier
pIdentifier = AST.Identifier <$> (lexeme . try) (pCandidate >>= check)
 where
  pCandidate =
    Text.pack
      <$> ((:) <$> MC.letterChar <*> many MC.alphaNumChar <?> "identifier")
  check x = if HashSet.member x reservedKeywords
    then fail $ "keyword " <> show x <> " cannot be an identifier"
    else return x

pValue :: Parser Expression
pValue = Value <$> pIdentifier

operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ [prefix "-" Negate, prefix "+" id]
  , [binary "*" AST.multiply, binary "/" AST.divide]
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
pTerm = choice [pInteger, pBool, pValue]

pExpression :: Parser Expression
pExpression = makeExprParser pTerm operatorTable <|> pIfElse <|> pVal

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

pVal :: Parser Expression
pVal = do
  pKeyword "val"
  lineNum    <- (MP.unPos . MP.sourceLine) <$> getSourcePos
  identifier <- pIdentifier
  symbol "="
  boundValue <- pExpression
  body       <- pExpression
  return $ ValBinding $ AST.ValBinding' identifier lineNum boundValue body
