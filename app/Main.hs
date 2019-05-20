module Main where

import           Lib

main :: IO ()
main = do
    putStrLn "Please enter the old full area:"
    oldFullArea <- getInput
    putStrLn "Please enter the old area:"
    oldArea <- getInput
    putStrLn "Please enter the new full area:"
    newFullArea <- getInput
    let newArea = convertArea oldFullArea newFullArea oldArea
    putStrLn "I think the new area is:"
    printArea newArea
