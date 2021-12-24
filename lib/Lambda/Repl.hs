module Lambda.Repl where

import Data.List
import Control.Monad.Trans
import Lambda.Parsing
import System.Console.Repline

type Repl a = HaskelineT IO a

-- Evaluation : handle each line user inputs
cmd :: String -> Repl ()
cmd input = liftIO $ print $ parseExpression input

completer :: Monad m => WordCompleter m
completer n = do
  let names = []
  return $ filter (isPrefixOf n) names

-- Commands
help :: [String] -> Repl ()
help args = liftIO $ print $ "Help: " ++ show args

opts :: [(String, String -> Repl ())]
opts =
  [ ("help", help . words) -- :help
  ]

ini :: Repl ()
ini = liftIO $ putStrLn "Welcome to Lambda REPL!"

final :: Repl ExitDecision
final = do
  liftIO $ putStrLn "Goodbye!"
  return Exit

repl :: IO ()
repl = evalReplOpts $ ReplOpts
  { banner           = const $ pure "λ> "
  , command          = cmd
  , options          = opts
  , prefix           = Just ':'
  , multilineCommand = Just "paste"
  , tabComplete      = Word0 completer
  , initialiser      = ini
  , finaliser        = final
  }
