module Elements.Compiler
  ( compile
  , compileExpression
  )
where

import qualified Data.Aeson.Encode.Pretty      as AesonPretty
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.ByteString.Internal      as BS
import qualified Data.Text.IO                  as TextIO
import qualified System.FilePath.Posix         as System
import qualified Text.Megaparsec               as Megaparsec

import qualified Elements.AST                as AST
import           Elements.Bytecode              ( Bytecode
                                                )
import qualified Elements.Bytecode           as Bytecode
import qualified Elements.Compiler.Fragment     as Fragment
import           Elements.Compiler.Fragment     ( Fragment )
import           Elements.Compiler.Types        ( Program(..) )
import           Elements.Parser                ( TextParseError
                                                , pExpression
                                                )

pushNumericValue :: AST.NumericValue -> Bytecode
pushNumericValue (AST.IntValue x) = Bytecode.PushInt x

arithmeticOpBytecode :: AST.ArithmeticOp -> Bytecode
arithmeticOpBytecode  = \case 
  AST.Add -> Bytecode.Add
  AST.Subtract -> Bytecode.Subtract

combineBinOpFragments
  :: Bytecode -> Fragment Bytecode -> Fragment Bytecode -> Fragment Bytecode
combineBinOpFragments opBytecode lhsFragment rhsFragment =
  Fragment.append opBytecode $ lhsFragment <> rhsFragment

bytecode :: Bytecode -> Fragment Bytecode
bytecode = Fragment.singleton

compileExpression :: AST.Expression -> Fragment Bytecode
compileExpression = \case
  AST.NumericLiteral value -> bytecode $ pushNumericValue value
  AST.BinaryOp (AST.BinaryOp' op lhs rhs) ->
    combineBinOpFragments 
      (arithmeticOpBytecode op)
      (compileExpression lhs)
      (compileExpression rhs)
  AST.Negate expr ->
    Fragment.append Bytecode.Negate $ compileExpression expr

parseFromFile :: FilePath -> IO (Either TextParseError AST.Expression)
parseFromFile filePath =
  Megaparsec.runParser pExpression filePath <$> TextIO.readFile filePath

compile :: FilePath -> IO ()
compile sourcePath = do
  expression <- parseFromFile sourcePath >>= \case
    Left  e -> error $ "could not parse file " <> sourcePath <> show e
    Right x -> return x
  let program = (Program . Fragment.toList . compileExpression) expression
      newline = BS.c2w '\n'
      output  = AesonPretty.encodePretty program `LBS.snoc` newline
      bytecodePath = System.addExtension (System.dropExtension sourcePath) ".json"
  LBS.writeFile bytecodePath output
