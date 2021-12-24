module Lambda.Evaluation where

import Lambda.Types

substitute :: Expression -> Expression -> Expression
substitute (Function (Name argumentName) body) argumentValue = substitute' body argumentName argumentValue
  where
    substitute' :: Expression -> String -> Expression -> Expression
    substitute' (Name name) argumentName argumentValue | name == argumentName = argumentValue
                                                       | otherwise            = Name name

    substitute' (Function (Name innerName) innerBody) argumentName argumentValue
      | innerName /= argumentName = Function (Name innerName) (substitute' innerBody argumentName argumentValue)
      | otherwise = Function (Name innerName) innerBody

    substitute' (Application func arg) argumentName argumentValue = Application (substitute' func argumentName argumentValue) (substitute' arg argumentName argumentValue)

substitute _ _ = error "Can't substitute into non-function expression"

eval :: Expression -> Expression
eval (Name name) = Name name
eval (Function argument body) = Function argument body
eval (Application func arg) | isFunction $ eval func = substitute (eval func) (eval arg)
                            | otherwise = Application (eval func) (eval arg)
