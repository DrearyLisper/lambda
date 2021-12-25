module Lambda.Evaluation where

import Lambda.Types

import qualified Data.Set as Set

beta :: Expression -> Expression -> Expression
beta (Function (Name argumentName) body) argumentValue = beta' body argumentName argumentValue
  where
    beta' :: Expression -> String -> Expression -> Expression
    beta' (Name name) argumentName argumentValue | name == argumentName = argumentValue
                                                 | otherwise            = Name name

    beta' (Function (Name innerName) innerBody) argumentName argumentValue
      | innerName /= argumentName = Function (Name innerName) (beta' innerBody argumentName argumentValue)
      | otherwise = Function (Name innerName) innerBody

    beta' (Application func arg) argumentName argumentValue = Application (beta' func argumentName argumentValue) (beta' arg argumentName argumentValue)
beta _ _ = error "Can't substitute into non-function expression"

free :: Expression -> [String]
free expression = free' expression Set.empty
  where
    free' :: Expression -> Set.Set String -> [String]
    free' (Name a) bound = [a | not (a `Set.member` bound)]
    free' (Function (Name argument) body) bound = free' body (argument `Set.insert` bound)
    free' (Function argument body) bound = error "Function argument can be Name String only"
    free' (Application function argument) bound = free' function bound ++ free' argument bound

eval :: Expression -> Expression
eval (Name name) = Name name
eval (Function argument body) = Function argument (eval body)
eval (Application function argument) | isFunction $ eval function = beta (eval function) argument
                                     | otherwise = Application (eval function) (eval argument)
