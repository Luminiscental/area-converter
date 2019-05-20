module Lib
    ( Area(Area)
    , Pair(Pair)
    , getAreaInput
    , convertArea
    , inputPrompt
    )
where

import           System.IO
import           Data.List
import           Text.Printf

data Pair = Pair Double Double
data Area = Area Pair Pair

instance Show Pair where
    show (Pair x y) = printf "(%.6f, %.6f)" x y

instance Eq Pair where
    (Pair x1 y1) == (Pair x2 y2) = x1 == x2 && y1 == y2

instance Show Area where
    show (Area offset dims) =
        "(left, top) -> (right, bottom) : "
            ++ show offset
            ++ " -> "
            ++ (show . addPairs dims $ offset)

instance Eq Area where
    (Area o1 d1) == (Area o2 d2) = o1 == o2 && d1 == d2

getScale :: Pair -> Pair -> Pair
getScale (Pair oldWidth oldHeight) (Pair newWidth newHeight) =
    let widthScale  = newWidth / oldWidth
        heightScale = newHeight / oldHeight
    in  Pair widthScale heightScale

getAreaInput :: IO Area
getAreaInput = do
    inputLine <- getLine
    let inputWords = words inputLine
    if length inputWords /= 4
        then do
            putStrLn "that wasn't 4 values!"
            getAreaInput
        else do
            let [left, top, right, bottom] = map read inputWords
            return $ Area (Pair left top) (Pair (right - left) (bottom - top))

inputPrompt :: String -> IO ()
inputPrompt msg = do
    putStrLn msg
    putStr "> "
    hFlush stdout

scalePairs :: Pair -> Pair -> Pair
scalePairs (Pair x1 y1) (Pair x2 y2) = Pair (x1 * x2) (y1 * y2)

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
