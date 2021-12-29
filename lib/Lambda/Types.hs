module Lambda.Types where

data Expression = Name String (Maybe Int)
                | Function Expression Expression (Maybe Int)
                | Application Expression Expression (Maybe Int)
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
