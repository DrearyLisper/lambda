-- |

module Lambda.Coding where

import Lambda.Types

number :: Int -> Expression
number i = Function (Name "f") (Function (Name "x") (f i))
  where
    f 0 = Name "x"
    f i = Application (Name "f") (f (i-1))
