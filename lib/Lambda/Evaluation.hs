{-# LANGUAGE RecordWildCards #-}
module Lambda.Evaluation where

import Lambda.Types
import Lambda.Coding ( number )

import Debug.Trace

import Data.List
import Text.Read (readMaybe)

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Lambda.Parsing (formatExpression)


data BetaContext = BetaContext {argumentName :: String,
                                argumentValue :: Expression,
                                replacementName :: Map.Map String String}

beta :: Expression -> Expression -> Expression
beta (Function (Name argumentName _) body _) argumentValue =
  beta' body (BetaContext argumentName argumentValue Map.empty)
  where
    freeArgNames = free argumentValue

    beta' :: Expression -> BetaContext -> Expression
    beta' (Name name _) BetaContext{..} | name == argumentName = argumentValue
                                        | name `Map.member` replacementName = Name (fromMaybe name (name `Map.lookup` replacementName)) Nothing
                                        | otherwise = Name name Nothing

    beta' (Application function argument _) betaContext = Application (beta' function betaContext) (beta' argument betaContext) Nothing

    beta' (Function (Name innerName uid) innerBody _) BetaContext{..}
      | innerName /= argumentName = Function (Name (newName innerName) uid)
                                             (beta' innerBody BetaContext{replacementName = Map.insert innerName (newName innerName) replacementName, ..})
                                             Nothing
      | otherwise = Function (Name innerName uid) innerBody Nothing
      where
        newName candidate | candidate `Set.member` freeArgNames || candidate `Map.member` replacementName = newName ("r-" ++ candidate)
                          | otherwise = candidate


beta _ _ = error "Can't substitute into non-function expression"

free :: Expression -> Set.Set String
free expression = free' expression Set.empty
  where
    free' :: Expression -> Set.Set String -> Set.Set String
    free' (Name a _) bound = Set.fromList [a | not (a `Set.member` bound)]
    free' (Function (Name argument _) body _) bound = free' body (argument `Set.insert` bound)
    free' (Function argument body _) bound = error "Function argument can be Name String only"
    free' (Application function argument _) bound = free' function bound `Set.union` free' argument bound

type StepDB = Map.Map String Expression

step :: (Expression, Bool, StepDB) -> (Expression, Bool, StepDB)
step (Name name uid, _, db)
  | isJust (readMaybe name :: Maybe Int) = (number (read name :: Int), True, db)
  | otherwise = case name `Map.lookup` db of
                  Nothing -> (Name name uid, False, db)
                  Just replacement -> (replacement, True, db)


step (Function argument body uid, _, db) = let (newBody, updated, db') = step (body, False, db)
                                           in (Function argument newBody uid, updated, db')

step (Application function argument uid, _, db)
  | isFunction function = let betaResult = beta function argument
                          in (betaResult, True, db)

  | otherwise = let (newFunction, updatedFunction, newDB) = step (function, False, db)
                    (newArgument, updatedArgument, newDB') = step (argument, False, db)
                in case (updatedFunction, updatedArgument) of
                     (False, False) -> (Application function argument uid, False, db)
                     (True, _) -> (Application newFunction argument uid, True, newDB)
                     (False, True) -> (Application function newArgument uid, True, newDB')

t (a,b,c) = (a,b,c) --trace (formatExpression a) (a,b,c)

eval :: (StepDB, Expression) -> (StepDB, Expression)
eval (db, expression) = (\(a,b,c)->(c,a)) $ head $ dropWhile (\(a,b,c)->b) $ iterate (step.t) (expression, True, db)
