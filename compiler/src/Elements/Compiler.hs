module Elements.Compiler
  ( compile
  , compileExpression
  ) where

import qualified Control.Monad.Except          as Except
import qualified Data.Aeson.Encode.Pretty      as AesonPretty
import qualified Data.ByteString.Internal      as BS
import qualified Data.ByteString.Lazy          as LBS
import qualified Data.HashMap.Strict           as HashMap
import qualified Data.Text.IO                  as TextIO
import qualified System.FilePath.Posix         as System
import qualified Text.Megaparsec               as Megaparsec

import qualified Elements.AST                  as AST
import           Elements.Bytecode              ( Bytecode
                                                , PCOffset(..)
                                                , Program(..)
                                                )
import qualified Elements.Bytecode             as Bytecode
import qualified Elements.Compiler.Fragment    as Fragment
import           Elements.Compiler.Fragment     ( Fragment )
import           Elements.Compiler.Types        ( CompileError(..)
                                                , DataType(..)
                                                , NumericType(..)
                                                , TypeError'(..)
                                                , VarTypes
                                                )
import qualified Elements.Compiler.Types       as CompilerT
import           Elements.Compiler.Vars         ( Vars )
import qualified Elements.Compiler.Vars        as Vars
import           Elements.Parser                ( TextParseError
                                                , pExpression
                                                )

codeGenNumericValue :: AST.NumericValue -> (DataType, Fragment Bytecode)
codeGenNumericValue = \case
  AST.IntValue x ->
    (NumericType IntType, Fragment.singleton $ Bytecode.PushInt x)
  AST.LongValue x ->
    (NumericType LongType, Fragment.singleton $ Bytecode.PushLong x)
  AST.DoubleValue x ->
    (NumericType DoubleType, Fragment.singleton $ Bytecode.PushDouble x)

arithmeticOpBytecode :: AST.ArithmeticOp -> (NumericType -> Bytecode)
arithmeticOpBytecode = \case
  AST.Add      -> Bytecode.Add
  AST.Subtract -> Bytecode.Subtract
  AST.Multiply -> Bytecode.Multiply
  AST.Divide   -> Bytecode.Divide

logicalOpBytecode :: AST.LogicalOp -> Bytecode
logicalOpBytecode = \case
  AST.And -> Bytecode.And
  AST.Or  -> Bytecode.Or

comparisonOpBytecode :: AST.ComparisonOp -> (NumericType -> Bytecode)
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

compileExpression
  :: Vars -> AST.Expression -> Either CompileError (DataType, Fragment Bytecode)
compileExpression vars = \case
  AST.NumericLiteral value -> Right $ codeGenNumericValue value

  AST.BoolLiteral    value -> Right
    (BoolType, Fragment.singleton $ Bytecode.PushInt $ if value then 1 else 0)

  AST.Value (AST.Value' i lNum) -> case Vars.lookupVar i vars of
    Just vInfo -> Right
      ( Vars.dataType vInfo
      , Fragment.singleton $ Bytecode.GetLocal $ Vars.localVarIndex vInfo
      )
    Nothing -> Left $ UndefinedValueError i lNum

  AST.UnaryMinus expr -> do
    (dataType, fragment) <- compileExpression vars expr
    case dataType of
      NumericType nt ->
        Right (dataType, Fragment.append (Bytecode.Negate nt) fragment)
      BoolType -> Left UndefinedOperatorError

  AST.BinaryArithOp (AST.BinaryArithOp' op lhs rhs) -> do
    (lType, lBytecodes) <- compileExpression vars lhs
    (rType, rBytecodes) <- compileExpression vars rhs
    case (lType, rType) of
      (NumericType lnt, NumericType rnt) | lnt == rnt -> Right
        ( lType
        , combineBinOpFragments (arithmeticOpBytecode op lnt)
                                lBytecodes
                                rBytecodes
        )
      _ -> Left UndefinedOperatorError

  AST.BinaryLogicalOp (AST.BinaryLogicalOp' op lhs rhs) -> do
    (lType, lBytecodes) <- compileExpression vars lhs
    (rType, rBytecodes) <- compileExpression vars rhs
    case (lType, rType) of
      (BoolType, BoolType) -> Right
        ( BoolType
        , combineBinOpFragments (logicalOpBytecode op) lBytecodes rBytecodes
        )
      _ -> Left UndefinedOperatorError

  AST.Comparison (AST.Comparison' op lhs rhs) -> do
    (lType, lBytecodes) <- compileExpression vars lhs
    (rType, rBytecodes) <- compileExpression vars rhs
    case (lType, rType) of
      (NumericType lnt, NumericType rnt) -> Right
        ( BoolType
        , combineBinOpFragments (comparisonOpBytecode op lnt)
                                lBytecodes
                                rBytecodes
        )
      _ -> Left UndefinedOperatorError

  AST.IfElse (AST.IfElse' testExpr thenExpr elseExpr) -> do
    (testType, testBytecodes) <- compileExpression vars testExpr
    (thenType, thenBytecodes) <- compileExpression vars thenExpr
    (elseType, elseBytecodes) <- compileExpression vars elseExpr
    if testType == BoolType
      then if thenType == elseType
        then Right
          ( thenType
          , combineIfElseFragments testBytecodes thenBytecodes elseBytecodes
          )
        else Left $ TypeError $ CompilerT.TypeError' thenType elseType
      else Left $ TypeError $ CompilerT.TypeError' BoolType testType

  AST.ValBinding (AST.ValBinding' identifier lNum boundExpr baseExpr) ->
    case Vars.lookupVar identifier vars of
      Just varInfo ->
        Left
          $ DuplicateValueDefinitionError
          $ CompilerT.DuplicateValueDefinitionError' identifier
                                                     (Vars.lineNum varInfo)
                                                     lNum
      Nothing -> do
        (boundType, boundBytecodes) <- compileExpression vars boundExpr
        let (n, newVars) = Vars.addVar identifier lNum boundType vars
            storeLocal   = Bytecode.StoreLocal n
        (baseType, baseBytecodes) <- compileExpression newVars baseExpr
        Right
          (baseType, Fragment.append storeLocal boundBytecodes <> baseBytecodes)

parseFromFile :: FilePath -> IO (Either TextParseError AST.Expression)
parseFromFile filePath =
  Megaparsec.runParser pExpression filePath <$> TextIO.readFile filePath

printCompilerError :: CompileError -> String
printCompilerError = \case
  TypeError (CompilerT.TypeError' expected actual) ->
    "type error, expected value of type "
      <> CompilerT.showDataType expected
      <> " but the actual type is "
      <> CompilerT.showDataType actual

  DuplicateValueDefinitionError (CompilerT.DuplicateValueDefinitionError' i o d)
    -> let quotedIdentifier = show $ AST.unwrapIndentifier i
       in
         "the value definition "
         <> quotedIdentifier
         <> " in line "
         <> show d
         <> " attempts to define a value that has already been defined in line "
         <> show o

  UndefinedValueError i lNum ->
    let quotedIdentifier = show $ AST.unwrapIndentifier i
    in  "the value "
          <> quotedIdentifier
          <> " in line "
          <> show lNum
          <> " has not been defined"
  
  UndefinedOperatorError -> "the operator is not defined"

compile :: FilePath -> IO ()
compile sourcePath = parseFromFile sourcePath >>= \case
  Left  e          -> error $ "could not parse file " <> sourcePath <> show e
  Right expression -> case compileExpression Vars.empty expression of
    Left e ->
      error
        $  "could not compile file "
        <> sourcePath
        <> ", "
        <> printCompilerError e
    Right (_, bytecodes) ->
      let
        program = Program $ Fragment.toList bytecodes
        newline = BS.c2w '\n'
        output  = AesonPretty.encodePretty program `LBS.snoc` newline
        bytecodePath =
          System.addExtension (System.dropExtension sourcePath) ".json"
      in
        LBS.writeFile bytecodePath output
