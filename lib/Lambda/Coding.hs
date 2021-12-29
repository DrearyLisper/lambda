-- |

module Lambda.Coding where

import Lambda.Types

number :: Int -> Expression
number i = Function (Name "f" Nothing) (Function (Name "x" Nothing) (f i) Nothing) Nothing
  where
    f 0 = Name "x" Nothing
    f i = Application (Name "f" Nothing) (f (i-1)) Nothing
