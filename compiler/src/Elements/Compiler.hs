module Elements.Compiler
  ( compile
  , compileExpression
  ) where

import qualified Control.Monad.Except          as Except
import qualified Data.Aeson.Encode.Pretty      as AesonPretty
import qualified Data.ByteString.Internal      as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.Text.IO                  as TextIO
import qualified System.FilePath.Posix         as System
import qualified Text.Megaparsec               as Megaparsec

import qualified Elements.AST                  as AST
import           Elements.Bytecode              ( Bytecode
                                                , PCOffset(..)
                                                )
import qualified Elements.Bytecode             as Bytecode
import qualified Elements.Compiler.Fragment    as Fragment
import           Elements.Compiler.Fragment     ( Fragment )
import           Elements.Compiler.Types        ( CompileError(..)
                                                , CompilerM
                                                , Program(..)
                                                )
import qualified Elements.Compiler.Types       as CompilerT
import           Elements.Compiler.Vars         ( Vars )
import qualified Elements.Compiler.Vars        as Vars
import           Elements.Parser                ( TextParseError
                                                , pExpression
                                                )

pushNumericValue :: AST.NumericValue -> Bytecode
pushNumericValue (AST.IntValue x) = Bytecode.PushInt x

arithmeticOpBytecode :: AST.ArithmeticOp -> Bytecode
arithmeticOpBytecode = \case
  AST.Add      -> Bytecode.Add
  AST.Subtract -> Bytecode.Subtract
  AST.Multiply -> Bytecode.Multiply
  AST.Divide   -> Bytecode.Divide

comparisonOpBytecode :: AST.ComparisonOp -> Bytecode
comparisonOpBytecode = \case
  AST.LessThan            -> Bytecode.LessThan
  AST.LessThanOrEquals    -> Bytecode.LessThanOrEquals
  AST.Equals              -> Bytecode.Equals
  AST.GreaterThanOrEquals -> Bytecode.GreaterThanOrEquals
  AST.GreaterThan         -> Bytecode.GreaterThan
  AST.NotEquals           -> Bytecode.NotEquals

combineBinOpFragments
  :: Bytecode -> Fragment Bytecode -> Fragment Bytecode -> Fragment Bytecode
combineBinOpFragments opBytecode lhsFragment rhsFragment =
  Fragment.append opBytecode $ lhsFragment <> rhsFragment

bytecode :: Bytecode -> CompilerM (Fragment Bytecode)
bytecode = return . Fragment.singleton

fragmentPCOffset :: Fragment a -> PCOffset
fragmentPCOffset = PCOffset . fromIntegral . Fragment.length

combineIfElseFragments
  :: Fragment Bytecode
  -> Fragment Bytecode
  -> Fragment Bytecode
  -> Fragment Bytecode
combineIfElseFragments testFragment thenFragment elseFragment =
  let branchToElse =
        (Bytecode.BranchIfFalse . (+) 1 . fragmentPCOffset) thenFragment
      gotoElseEnd = (Bytecode.Goto . fragmentPCOffset) elseFragment
  in  Fragment.append branchToElse testFragment
        <> Fragment.append gotoElseEnd thenFragment
        <> elseFragment

compileExpression :: Vars -> AST.Expression -> CompilerM (Fragment Bytecode)
compileExpression vars = \case
  AST.NumericLiteral value -> bytecode $ pushNumericValue value

  AST.BoolLiteral value -> bytecode $ Bytecode.PushInt $ if value then 1 else 0

  AST.Value x -> case Vars.lookupVar x vars of
    Just vInfo -> bytecode $ Bytecode.GetLocal $ Vars.localVarIndex vInfo
    Nothing    -> Except.throwError $ UndefinedValueError x

  AST.Negate expr ->
    Fragment.append Bytecode.Negate <$> compileExpression vars expr

  AST.BinaryOp (AST.BinaryOp' op lhs rhs) ->
    combineBinOpFragments (arithmeticOpBytecode op)
      <$> compileExpression vars lhs
      <*> compileExpression vars rhs

  AST.Comparison (AST.Comparison' op lhs rhs) ->
    combineBinOpFragments (comparisonOpBytecode op)
      <$> compileExpression vars lhs
      <*> compileExpression vars rhs

  AST.IfElse (AST.IfElse' testCondition thenExpr elseExpr) ->
    combineIfElseFragments
      <$> compileExpression vars testCondition
      <*> compileExpression vars thenExpr
      <*> compileExpression vars elseExpr

  AST.ValBinding (AST.ValBinding' identifier lNum boundExpr baseExpr) ->
    case Vars.lookupVar identifier vars of
      Just varInfo ->
        Except.throwError
          $ DuplicateValueDefinitionError
          $ CompilerT.DuplicateValueDefinitionError' identifier
                                                     (Vars.lineNum varInfo)
                                                     lNum
      Nothing ->
        let (n, newVars) = Vars.addVar identifier lNum vars
            storeLocal   = Bytecode.StoreLocal n
        in  (\boundFragment baseFragment ->
              Fragment.append storeLocal boundFragment <> baseFragment
            )
              <$> compileExpression vars    boundExpr
              <*> compileExpression newVars baseExpr

parseFromFile :: FilePath -> IO (Either TextParseError AST.Expression)
parseFromFile filePath =
  Megaparsec.runParser pExpression filePath <$> TextIO.readFile filePath

printCompilerError :: CompileError -> String
printCompilerError = \case
  DuplicateValueDefinitionError (CompilerT.DuplicateValueDefinitionError' i o d)
    -> let quotedIdentifier = show $ AST.unwrapIndentifier i
       in
         "the value definition "
         <> quotedIdentifier
         <> " in line "
         <> show d
         <> " attempts to define a value that has already been defined in line "
         <> show o

  UndefinedValueError i ->
    let quotedIdentifier = show $ AST.unwrapIndentifier i
    in  "the value " <> quotedIdentifier <> " has not been defined"

compile :: FilePath -> IO ()
compile sourcePath = do
  expression <- parseFromFile sourcePath >>= \case
    Left  e -> error $ "could not parse file " <> sourcePath <> show e
    Right x -> return x
  let
    program =
      case CompilerT.runCompilerM (compileExpression Vars.empty expression) of
        Left e ->
          error
            $  "could not compile file "
            <> sourcePath
            <> ", "
            <> printCompilerError e
        Right fragment -> (Program . Fragment.toList) fragment
    newline = BS.c2w '\n'
    output  = AesonPretty.encodePretty program `LBS.snoc` newline
    bytecodePath =
      System.addExtension (System.dropExtension sourcePath) ".json"
  LBS.writeFile bytecodePath output
