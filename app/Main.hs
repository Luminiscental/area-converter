module Main where

import           Lib

main :: IO ()
main = do
    putStrLn
        "Area values are taken as <left value> <top value> <right value> <bottom value>"
    putStrLn "Please enter the old full area"
    oldFullArea <- getAreaInput
    putStrLn "Please enter the old area"
    oldArea <- getAreaInput
    putStrLn "Please enter the new full area"
    newFullArea <- getAreaInput
    let newArea = convertArea oldFullArea newFullArea oldArea
    putStrLn "I think the new area is"
    print newArea
