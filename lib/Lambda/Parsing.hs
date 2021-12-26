module Lambda.Parsing where

import Lambda.Types

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


formatExpression :: Expression -> String
formatExpression (Name name) = name
formatExpression (Application func arg) = "(" ++
                                          formatExpression func ++
                                          " " ++
                                          formatExpression arg ++
                                          ")"
formatExpression (Function arg body) = "\\" ++
                                       formatExpression arg ++
                                       "." ++
                                       formatExpression body
