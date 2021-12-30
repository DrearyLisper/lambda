module Lambda.Types where

import qualified Data.Map.Strict as Map

type StepDB = Map.Map String Expression

data Expression = Name String
                | Function Expression Expression
                | Application Expression Expression
                deriving (Show, Eq, Ord)

isName :: Expression -> Bool
isName Name {} = True
isName _ = False

isFunction :: Expression -> Bool
isFunction Function {} = True
isFunction _ = False

isApplication :: Expression -> Bool
isApplication Application {} = True
isApplication _ = False
