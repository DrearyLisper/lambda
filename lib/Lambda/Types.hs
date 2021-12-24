module Lambda.Types where

data Expression = Name String | Function Expression Expression | Application Expression Expression deriving Show

isName :: Expression -> Bool
isName (Name _) = True
isName _ = False

isFunction :: Expression -> Bool
isFunction (Function _ _) = True
isFunction _ = False

isApplication :: Expression -> Bool
isApplication (Application _ _) = True
isApplication _ = False
