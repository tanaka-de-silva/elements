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
                                                )
import qualified Elements.Bytecode             as Bytecode
import qualified Elements.Compiler.Fragment    as Fragment
import           Elements.Compiler.Fragment     ( Fragment )
import           Elements.Compiler.Types        ( CompileError(..)
                                                , DataType(..)
                                                , Program(..)
                                                , TypeError'(..)
                                                , VarTypes
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

logicalOpBytecode :: AST.LogicalOp -> Bytecode
logicalOpBytecode = \case
  AST.And -> Bytecode.And
  AST.Or  -> Bytecode.Or

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

bytecode :: Bytecode -> Fragment Bytecode
bytecode = Fragment.singleton

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

assertExpressionType
  :: VarTypes -> DataType -> AST.Expression -> Either CompileError DataType
assertExpressionType varTypes expected expr = do
  actual <- checkExpression varTypes expr
  if actual == expected
    then Right expected
    else Left $ TypeError $ TypeError' expected actual

checkExpression :: VarTypes -> AST.Expression -> Either CompileError DataType
checkExpression varTypes = \case
  AST.NumericLiteral _ -> Right IntType

  AST.BoolLiteral    _ -> Right BoolType

  AST.Value          i -> case HashMap.lookup i varTypes of
    Nothing  -> Left $ UndefinedValueError i
    Just vti -> Right $ CompilerT.vtDataType vti

  AST.Negate expr -> assertExpressionType varTypes IntType expr

  AST.BinaryArithOp (AST.BinaryArithOp' _ lhs rhs) ->
    (\_ _ -> IntType)
      <$> assertExpressionType varTypes IntType lhs
      <*> assertExpressionType varTypes IntType rhs

  AST.BinaryLogicalOp (AST.BinaryLogicalOp' _ lhs rhs) ->
    (\_ _ -> BoolType)
      <$> assertExpressionType varTypes BoolType lhs
      <*> assertExpressionType varTypes BoolType rhs

  AST.Comparison (AST.Comparison' op lhs rhs) ->
    if op `elem` [AST.Equals, AST.NotEquals]
      then do
        lhsType <- checkExpression varTypes lhs
        rhsType <- checkExpression varTypes rhs
        if lhsType == rhsType
          then Right BoolType
          else Left $ TypeError $ TypeError' lhsType rhsType
      else
        (\_ _ -> BoolType)
        <$> assertExpressionType varTypes IntType lhs
        <*> assertExpressionType varTypes IntType rhs


  AST.IfElse (AST.IfElse' testCondition thenExpr elseExpr) -> do
    assertExpressionType varTypes BoolType testCondition
    thenType <- checkExpression varTypes thenExpr
    elseType <- checkExpression varTypes elseExpr
    if thenType == elseType
      then Right thenType
      else Left $ TypeError $ TypeError' thenType elseType

  AST.ValBinding (AST.ValBinding' identifier lNum boundExpr baseExpr) -> do
    case HashMap.lookup identifier varTypes of
      Just varTypeInfo ->
        Left
          $ DuplicateValueDefinitionError
          $ CompilerT.DuplicateValueDefinitionError'
              identifier
              (CompilerT.vtLineNum varTypeInfo)
              lNum
      Nothing -> do
        boundType <- checkExpression varTypes boundExpr
        let newVarTypeInfo = CompilerT.VarTypeInfo boundType lNum
            modifiedVarTypes =
              HashMap.insert identifier newVarTypeInfo varTypes
        checkExpression modifiedVarTypes baseExpr

codegen :: Vars -> AST.Expression -> Fragment Bytecode
codegen vars = \case
  AST.NumericLiteral value -> bytecode $ pushNumericValue value

  AST.BoolLiteral value -> bytecode $ Bytecode.PushInt $ if value then 1 else 0

  AST.Value x -> case Vars.lookupVar x vars of
    Just vInfo -> bytecode $ Bytecode.GetLocal $ Vars.localVarIndex vInfo
    Nothing ->
      error $ "internal error, unexpected undefined value error " <> show x

  AST.Negate expr -> Fragment.append Bytecode.Negate $ codegen vars expr

  AST.BinaryArithOp (AST.BinaryArithOp' op lhs rhs) -> combineBinOpFragments
    (arithmeticOpBytecode op)
    (codegen vars lhs)
    (codegen vars rhs)

  AST.BinaryLogicalOp (AST.BinaryLogicalOp' op lhs rhs) ->
    combineBinOpFragments (logicalOpBytecode op)
                          (codegen vars lhs)
                          (codegen vars rhs)

  AST.Comparison (AST.Comparison' op lhs rhs) -> combineBinOpFragments
    (comparisonOpBytecode op)
    (codegen vars lhs)
    (codegen vars rhs)

  AST.IfElse (AST.IfElse' testCondition thenExpr elseExpr) ->
    combineIfElseFragments (codegen vars testCondition)
                           (codegen vars thenExpr)
                           (codegen vars elseExpr)

  AST.ValBinding (AST.ValBinding' identifier lNum boundExpr baseExpr) ->
    case Vars.lookupVar identifier vars of
      Just _ ->
        error $ "internal error, unexpected duplicate value error, " <> show
          (AST.unwrapIndentifier identifier)
      Nothing ->
        let (n, newVars) = Vars.addVar identifier lNum vars
            storeLocal   = Bytecode.StoreLocal n
        in  Fragment.append storeLocal (codegen vars boundExpr)
              <> (codegen newVars baseExpr)

parseFromFile :: FilePath -> IO (Either TextParseError AST.Expression)
parseFromFile filePath =
  Megaparsec.runParser pExpression filePath <$> TextIO.readFile filePath

printCompilerError :: CompileError -> String
printCompilerError = \case
  TypeError (CompilerT.TypeError' expected actual) ->
    "type error, expected value of type "
      <> show expected
      <> " but the actual type is "
      <> show actual

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

compileExpression :: AST.Expression -> Either CompileError (Fragment Bytecode)
compileExpression expression = case checkExpression HashMap.empty expression of
  Left  e -> Left e
  Right _ -> Right $ codegen Vars.empty expression

compile :: FilePath -> IO ()
compile sourcePath = parseFromFile sourcePath >>= \case
  Left  e          -> error $ "could not parse file " <> sourcePath <> show e
  Right expression -> case compileExpression expression of
    Left e ->
      error
        $  "could not compile file "
        <> sourcePath
        <> ", "
        <> printCompilerError e
    Right bytecodes ->
      let
        program = Program $ Fragment.toList bytecodes
        newline = BS.c2w '\n'
        output  = AesonPretty.encodePretty program `LBS.snoc` newline
        bytecodePath =
          System.addExtension (System.dropExtension sourcePath) ".json"
      in
        LBS.writeFile bytecodePath output
