{-# LANGUAGE RecordWildCards #-}
module Lambda.Evaluation where

import Lambda.Types
import Lambda.Coding ( number )

import Debug.Trace

import Data.List
import Text.Read (readMaybe)

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Lambda.Parsing (formatExpression)


data BetaContext = BetaContext {argumentName :: String,
                                argumentValue :: Expression,
                                replacementName :: Map.Map String String}

beta :: Expression -> Expression -> Expression
beta (Function (Name argumentName) body) argumentValue =
  beta' body (BetaContext argumentName argumentValue Map.empty)
  where
    freeArgNames = free argumentValue

    beta' :: Expression -> BetaContext -> Expression
    beta' (Name name) BetaContext{..} | name == argumentName = argumentValue
                                      | name `Map.member` replacementName = Name (fromMaybe name (name `Map.lookup` replacementName))
                                      | otherwise = Name name

    beta' (Application function argument) betaContext = Application (beta' function betaContext) (beta' argument betaContext)

    beta' (Function (Name innerName) innerBody) BetaContext{..}
      | innerName /= argumentName = Function (Name (newName innerName))
                                             (beta' innerBody BetaContext{replacementName = Map.insert innerName (newName innerName) replacementName, ..})
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

retrieveFunction :: StepDB -> Expression -> Expression
retrieveFunction _ (Function argument body) = Function argument body
retrieveFunction _ (Application function argument) = Application function argument
retrieveFunction db (Name name) = case name `Map.lookup` db of
                                    Nothing -> Name name
                                    Just replacement -> replacement


step :: (Expression, Bool, StepDB) -> (Expression, Bool, StepDB)
step (Name name, _, db)
  | isJust (readMaybe name :: Maybe Int) = (number (read name :: Int), True, db)
  | otherwise = case name `Map.lookup` db of
                  Nothing -> (Name name, False, db)
                  Just replacement -> let (replacement', updated, db') = step (replacement, False, db)
                                      in if updated
                                            then (Name name, True, Map.insert name replacement' db')
                                            else (replacement', True, db')

step (Function argument body, _, db) = let (newBody, updated, db') = step (body, False, db)
                                       in (Function argument newBody, updated, db')

step (Application function argument, _, db)
  | isFunction (retrieveFunction db function) = let betaResult = beta (retrieveFunction db function) argument
                                                in (betaResult, True, db)

  | otherwise = let (newFunction, updatedFunction, newDB) = step (function, False, db)
                    (newArgument, updatedArgument, newDB') = step (argument, False, db)
                in case (updatedFunction, updatedArgument) of
                     (False, False) -> (Application function argument, False, db)
                     (True, _) -> (Application newFunction argument, True, newDB)
                     (False, True) -> (Application function newArgument, True, newDB')

t (a,b,c) = (a,b,c) -- trace (formatExpression a) (a,b,c)

eval :: (StepDB, Expression) -> (StepDB, Expression)
eval (db, expression) = (\(a,b,c)->(c,a)) $ head $ dropWhile (\(a,b,c)->b) $ iterate (step.t) (expression, True, db)
