module Lib
    ( Dims
    , Offset
    , getInput
    , convertArea
    , printArea
    )
where

import           Data.List
import           Text.Printf

data Dims = Dims Double Double

getWidth :: Dims -> Double
getWidth (Dims w h) = w

getHeight :: Dims -> Double
getHeight (Dims w h) = h

getScale :: Dims -> Dims -> Dims
getScale origDims newDims =
    let widthScale  = getWidth newDims / getWidth origDims
        heightScale = getHeight newDims / getHeight origDims
    in  Dims widthScale heightScale

data Offset = Offset Double Double

instance Show Offset where
    show (Offset x y) = printf "(%.6f, %.6f)" x y

xOffset :: Offset -> Double
xOffset (Offset x y) = x

yOffset :: Offset -> Double
yOffset (Offset x y) = y

getInput :: IO (Offset, Dims)
getInput = do
    putStrLn
        "enter the dimensions as <left value> <top value> <right value> <bottom value>"
    inputLine <- getLine
    let inputWords = words inputLine
    if length inputWords /= 4
        then do
            putStrLn "that wasn't 4 values!"
            getInput
        else do
            let [left, top, right, bottom] = map read inputWords
            return (Offset left top, Dims (right - left) (bottom - top))

scaleOffsetBy :: Dims -> Offset -> Offset
scaleOffsetBy (Dims w h) (Offset x y) = Offset (x * w) (y * h)

addOffset :: Offset -> Offset -> Offset
addOffset (Offset x1 y1) (Offset x2 y2) = Offset (x1 + x2) (y1 + y2)

addDimsToOffset :: Dims -> Offset -> Offset
addDimsToOffset (Dims w h) (Offset x y) = Offset (x + w) (y + h)

scaleBy :: Dims -> Dims -> Dims
scaleBy (Dims scaleW scaleH) (Dims w h) = Dims (scaleW * w) (scaleH * h)

printArea :: (Offset, Dims) -> IO ()
printArea (offset, dims) =
    putStrLn
        $  "(left, top) -> (right, bottom) : "
        ++ show offset
        ++ " -> "
        ++ (show . addDimsToOffset dims $ offset)

convertArea
    :: (Offset, Dims) -> (Offset, Dims) -> (Offset, Dims) -> (Offset, Dims)
convertArea (oldBaseOffset, oldBaseDims) (newBaseOffset, newBaseDims) (oldOffset, oldDims)
    = let scale    = getScale oldBaseDims newBaseDims
          oldStart = Offset (xOffset oldOffset - xOffset oldBaseOffset)
                            (yOffset oldOffset - yOffset oldBaseOffset)
      in  ( addOffset newBaseOffset (scaleOffsetBy scale oldStart)
          , scaleBy scale oldDims
          )
