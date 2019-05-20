module Main where

import           Lib
import           System.IO

main :: IO ()
main = do
    putStrLn
        "Area values are taken as <left value> <top value> <right value> <bottom value>"
    inputPrompt "Please enter the old full area:"
    oldFullArea <- getAreaInput
    inputPrompt "Please enter the old area:"
    oldArea <- getAreaInput
    inputPrompt "Please enter the new full area:"
    newFullArea <- getAreaInput
    let newArea = convertArea oldFullArea newFullArea oldArea
    putStrLn "I think the new area is:"
    print newArea
