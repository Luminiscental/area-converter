module Lib
    ( Area(Area)
    , Pair(Pair)
    , getAreaInput
    , convertArea
    )
where

import           System.IO
import           Text.Printf
import           Text.Read

data Pair = Pair Double Double
    deriving (Eq)
data Area = Area Pair Pair
    deriving (Eq)

instance Show Pair where
    show (Pair x y) = printf "(%.6f, %.6f)" x y

instance Show Area where
    show (Area offset dims) =
        "(left, top) -> (right, bottom) : "
            ++ show offset
            ++ " -> "
            ++ (show . addPairs dims $ offset)

promptInputLine :: IO String
promptInputLine = do
    putStr "> "
    hFlush stdout
    getLine

getAreaInput :: IO Area
getAreaInput = do
    inputLine <- promptInputLine
    let inputWords = words inputLine
    if length inputWords /= 4
        then do
            putStrLn "Please give 4 values, they can't contain spaces"
            getAreaInput
        else
            let wordList = mapM readMaybe inputWords
            in
                case wordList of
                    Just [left, top, right, bottom] -> return $ Area
                        (Pair left top)
                        (Pair (right - left) (bottom - top))
                    Just _  -> error "Length check failed"
                    Nothing -> do
                        putStrLn
                            "I couldn't interpret all those values as numbers"
                        getAreaInput

scalePairs :: Pair -> Pair -> Pair
scalePairs (Pair x1 y1) (Pair x2 y2) = Pair (x1 * x2) (y1 * y2)

getScale :: Pair -> Pair -> Pair
getScale (Pair oldWidth oldHeight) (Pair newWidth newHeight) =
    let widthScale  = newWidth / oldWidth
        heightScale = newHeight / oldHeight
    in  Pair widthScale heightScale

addPairs :: Pair -> Pair -> Pair
addPairs (Pair x1 y1) (Pair x2 y2) = Pair (x1 + x2) (y1 + y2)

subtractPairs :: Pair -> Pair -> Pair
subtractPairs (Pair x1 y1) (Pair x2 y2) = Pair (x1 - x2) (y1 - y2)

convertArea :: Area -> Area -> Area -> Area
convertArea (Area oldBaseOffset oldBaseDims) (Area newBaseOffset newBaseDims) (Area oldOffset oldDims)
    = let scale    = getScale oldBaseDims newBaseDims
          oldStart = subtractPairs oldOffset oldBaseOffset
      in  Area (addPairs newBaseOffset (scalePairs scale oldStart))
               (scalePairs scale oldDims)
