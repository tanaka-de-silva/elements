module Elements.Parser
  ( Parser
  , TextParseError
  , pExpression
  )
where

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

operatorTable :: [[Operator Parser Expression]]
operatorTable = [[binary "+" AST.add]]

pExpression :: Parser Expression
pExpression = makeExprParser pInteger operatorTable

binary
  :: Text
  -> (Expression -> Expression -> Expression)
  -> Operator Parser Expression
binary name f = InfixL (f <$ symbol name)
