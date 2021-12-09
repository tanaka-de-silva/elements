module Elements.Syntax where

import           Elements.AST                   ( Expression(NumericLiteral)
                                                , NumericValue(IntValue)
                                                )

int :: Int -> Expression
int = NumericLiteral . IntValue . fromIntegral
