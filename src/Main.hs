{-# LANGUAGE ScopedTypeVariables, DataKinds, RankNTypes #-}

module Main where

import System.Console.Repline
import Control.Monad.IO.Class
import Control.Monad.Except
import Parser
import Typecheck
import Type
import Syntax
import Pretty
import Eval
import Text.Printf

type Repl a = HaskelineT IO a

getTypedExpr :: String -> (forall t. STy t -> Expr '[] t -> a) -> Either String a
getTypedExpr input func =
  case parseExpr input of
    Left err -> Left (show err)
    Right x -> runExcept $ check x $ \ty expr -> return $ func ty expr

evalInput :: String -> Repl ()
evalInput input = 
  case eith of 
    Left err -> liftIO $ putStrLn err
    Right x -> liftIO $ putStrLn x
  where eith = getTypedExpr input (\ty expr -> printf "%s : %s" ((show.eval) expr) (show ty))

stepInput :: String -> Repl ()
stepInput input =
  case eith of 
    Left err -> liftIO $ putStrLn err
    Right x -> liftIO $ putStrLn x
  where eith = getTypedExpr input $ \ty expr -> loop ty expr
        loop ty expr = case step expr of
          Left val    -> printf "%s : %s" (show val) (show ty)
          Right expr' -> printf "--> %s\n%s" (show expr) (loop ty expr')

parseInput :: String -> Repl ()
parseInput input = case parseExpr input of
  Left err -> liftIO $ putStrLn $ "parse error: " ++ (show err)
  Right x  -> liftIO $ putStrLn $ "parsed expr: " ++ (show x)

typeInput :: String -> Repl ()
typeInput input =
  case eith of 
    Left err -> liftIO $ putStrLn err
    Right x -> liftIO $ putStrLn x
  where eith = getTypedExpr input $ \ty expr -> printf "%s : %s" (show expr) (show ty)

opts :: [(String, String -> Repl ())]
opts = [ ("eval",evalInput)
       , ("e",evalInput)
       , ("step",stepInput)
       , ("s",stepInput)
       , ("parse",parseInput)
       , ("p",parseInput)
       , ("type",typeInput)
       , ("t",typeInput)
       ]

initial :: Repl ()
initial = liftIO $ putStrLn "Welcome to STLC interpreter"

completer n = return []

repl :: IO ()
repl = evalReplOpts $ ReplOpts
    { banner           = const (pure ">>> ")
    , command          = evalInput
    , options          = opts
    , prefix           = Just ':'
    , multilineCommand = Nothing
    , tabComplete      = (Word0 completer)
    , initialiser      = initial
    , finaliser        = return Exit
    }

main = repl
