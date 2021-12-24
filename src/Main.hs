module Main where

import Data.List
import Control.Monad (forM_)
import Debug.Trace

import Lambda.Parsing
import Lambda.Evaluation

import System.Console.Repline

lambdas :: [String]
lambdas = ["(\\s.(s s) \\s.(s s))", "((\\first.\\second.second a) b)"]

main = forM_ (map (eval.eval.fst.parseExpression) lambdas) print
