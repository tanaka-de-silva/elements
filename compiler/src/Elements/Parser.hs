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

getLineNumber :: Parser AST.LineNumber
getLineNumber = (AST.LineNumber . MP.unPos . MP.sourceLine) <$> getSourcePos

pValue :: Parser Expression
pValue = do
  lineNumber <- getLineNumber
  identifier <- pIdentifier
  return $ Value $ AST.Value' identifier lineNumber

operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ [prefix "-" AST.UnaryMinus, prefix "+" id]
  , [binary "*" AST.multiply, binary "/" AST.divide]
  , [binary "+" AST.add, binary "-" AST.subtract]
  , [ binary "==" AST.equals
    , binary "!=" AST.notEquals
    , binary "<=" AST.lessThanOrEquals
    , binary "<"  AST.lessThan
    , binary ">=" AST.greaterThanOrEquals
    , binary ">"  AST.greaterThan
    ]
  , [binary "&&" AST.and, binary "||" AST.or]
  ]

pInteger :: Parser Expression
pInteger = NumericLiteral . AST.IntValue <$> lexeme
  (MCL.decimal <* notFollowedBy (MC.char 'L'))

pLong :: Parser Expression
pLong = NumericLiteral . AST.LongValue <$> lexeme (MCL.decimal <* MC.char 'L')

pDouble :: Parser Expression
pDouble = NumericLiteral . AST.DoubleValue <$> lexeme MCL.float

pNumeric :: Parser Expression
pNumeric = choice [try pDouble, try pInteger, pLong]

pTerm :: Parser Expression
pTerm = choice [pNumeric, pBool, pValue]

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
  lineNumber <- getLineNumber
  identifier <- pIdentifier
  symbol "="
  boundValue <- pExpression
  body       <- pExpression
  return $ ValBinding $ AST.ValBinding' identifier lineNumber boundValue body
