module Elements.Syntax where

import           Elements.AST                   ( Expression(NumericLiteral)
                                                , NumericValue(..)
                                                )

int :: Int -> Expression
int = NumericLiteral . IntValue . fromIntegral

long :: Int -> Expression
long = NumericLiteral . LongValue . fromIntegral

double :: Double -> Expression
double = NumericLiteral . DoubleValue
