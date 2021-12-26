{-# LANGUAGE RecordWildCards #-}
module Lambda.Evaluation where

import Lambda.Types

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust)


data BetaContext = BetaContext {argumentName :: String, argumentValue :: Expression, replacementName :: Map.Map String String}

beta :: Expression -> Expression -> Expression
beta (Function (Name argumentName) body) argumentValue = beta' body (BetaContext argumentName argumentValue Map.empty)
  where
    freeArgNames = free argumentValue

    beta' :: Expression -> BetaContext -> Expression
    beta' (Name name) BetaContext{..} | name == argumentName = argumentValue
                                      | name `Map.member` replacementName = Name $ fromMaybe name (name `Map.lookup` replacementName)
                                      | otherwise = Name name

    beta' (Application function argument) betaContext = Application (beta' function betaContext) (beta' argument betaContext)

    beta' (Function (Name innerName) innerBody) BetaContext{..}
      | innerName /= argumentName = Function (Name $ newName innerName) (beta' innerBody BetaContext{replacementName = Map.insert innerName (newName innerName) replacementName, ..})
      | otherwise = Function (Name innerName) innerBody
      where
        newName candidate | candidate `Set.member` freeArgNames || candidate `Map.member` replacementName = newName ("r-" ++ candidate)
                          | otherwise = candidate


beta _ _ = error "Can't substitute into non-function expression"

free :: Expression -> Set.Set String
free expression = free' expression Set.empty
  where
    free' :: Expression -> Set.Set String -> Set.Set String
    free' (Name a) bound = Set.fromList [a | not (a `Set.member` bound)]
    free' (Function (Name argument) body) bound = free' body (argument `Set.insert` bound)
    free' (Function argument body) bound = error "Function argument can be Name String only"
    free' (Application function argument) bound = free' function bound `Set.union` free' argument bound

step :: (Expression, Bool) -> (Expression, Bool)
step (Name name, _) = (Name name, False)
step (Function argument body, _) = let (newBody, updated) = step (body, False)
                                   in (Function argument newBody, updated)
step (Application function argument, _)
  | isFunction function = let newFunction = function
                          in (beta newFunction argument, True)
  | otherwise = let (newFunction, updatedFunction) = step (function, False)
                    (newArgument, updatedArgument) = step (argument, False)
                in case (updatedFunction, updatedArgument) of
                     (False, False) -> (Application function argument, False)
                     (True, _) -> (Application newFunction argument, True)
                     (False, True) -> (Application function newArgument, True)


evalOnce :: Expression -> Expression
evalOnce expression = fst $ step (expression, True)

eval :: Expression -> Expression
eval expression = fst $ head $ dropWhile snd $ iterate step (expression, True)
