module Main where

import           Lib
import           System.Environment             ( getArgs )
import           Control.Monad                  ( when )

main :: IO ()
main = do
    args <- getArgs
    if not . null $ args then displayCommands else run
  where
    run = do
        putStrLn
            "What command would you like to run? (type \"quit\" to exit, \"help\" to display supported commands)"
        command <- promptInputLine
        case command of
            "quit" -> return ()
            "help" -> displayCommands
            _      -> case getCommand command of
                Just c -> runCommand c
                Nothing ->
                    putStrLn $ "Couldn't find command \"" ++ command ++ "\""
        when (command /= "quit") run
