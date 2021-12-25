{-# LANGUAGE RecordWildCards #-}
module Lambda.Evaluation where

import Lambda.Types
import Debug.Trace

import qualified Data.Set as Set
import Data.Maybe (fromMaybe)

data BetaContext = BetaContext {argumentName :: String, argumentValue :: Expression, bond :: Maybe String, replacementName :: Maybe String}

beta :: Expression -> Expression -> Expression
beta (Function (Name argumentName) body) argumentValue = beta' body (BetaContext argumentName argumentValue Nothing Nothing)
  where
    freeArgNames = free argumentValue

    beta' :: Expression -> BetaContext -> Expression
    beta' (Name name) BetaContext{..} | name == argumentName = argumentValue
                                      | Just name == bond    = Name $ fromMaybe name replacementName
                                      | otherwise            = Name name

    beta' (Application function argument) betaContext = Application (beta' function betaContext) (beta' argument betaContext)

    beta' (Function (Name innerName) innerBody) BetaContext{..}
      | innerName /= argumentName = Function (Name name) (beta' innerBody BetaContext{bond = Just innerName, replacementName = Just name, ..})
      | otherwise = Function (Name innerName) innerBody
      where
        name = if innerName `Set.member` freeArgNames
                then "r-" ++ innerName
                else innerName


beta _ _ = error "Can't substitute into non-function expression"

free :: Expression -> Set.Set String
free expression = free' expression Set.empty
  where
    free' :: Expression -> Set.Set String -> Set.Set String
    free' (Name a) bound = Set.fromList [a | not (a `Set.member` bound)]
    free' (Function (Name argument) body) bound = free' body (argument `Set.insert` bound)
    free' (Function argument body) bound = error "Function argument can be Name String only"
    free' (Application function argument) bound = free' function bound `Set.union` free' argument bound

eval :: Expression -> Expression
eval (Name name) = Name name
eval (Function argument body) = Function argument (eval body)
eval (Application function argument) | isFunction $ eval function = beta (eval function) argument
                                     | otherwise = Application (eval function) (eval argument)
