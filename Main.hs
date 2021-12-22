module Main where

import Data.List
import Control.Monad (forM_)

import Debug.Trace

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

parseName :: String -> (Expression, String)
parseName xs = parseName' xs []
  where
    parseName' [] name = (Name $ reverse name, [])
    parseName' (x:xs) name | x `elem` ['.', '\\', '(', ')', ' '] = (Name $ reverse name, x:xs)
                           | otherwise                      = parseName' xs (x:name)

parseFunction :: String -> (Expression, String)
parseFunction xs = (Function argumentName functionBody, rest')
  where
    (argumentName, rest) = parseName xs
    (functionBody, rest') = parseExpression $ tail rest

parseApplication :: String -> (Expression, String)
parseApplication xs = (Application functionExpression argumentExpression, tail rest')
  where
    (functionExpression, rest) = parseExpression xs
    (argumentExpression, rest') = parseExpression $ tail rest

parseExpression :: String -> (Expression, String)
parseExpression [] = error "Empty string"
parseExpression (x:xs) | x == '\\' = parseFunction xs
                       | x == '('  = parseApplication xs
                       | otherwise = parseName (x:xs)

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

lambdas :: [String]
lambdas = ["(\\s.(s s) \\s.(s s))", "((\\first.\\second.second a) b)"]

main = forM_ (map (eval.eval.fst.parseExpression) lambdas) print
