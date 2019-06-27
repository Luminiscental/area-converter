module Lib
    ( Command
    , getCommand
    , displayCommands
    , runCommand
    , promptInputLine
    )
where

import           System.IO
import           Text.Printf
import           Text.Read

data Pair = Pair Double Double
    deriving (Eq)

-- |Represents area values for a driver:
-- > (Area origin dims)
-- where the origin is (left, top), and the dimensions are signed left->right, top->bottom.
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

-- * Arithmetic functions
--
-- Utility functions for dealing with pairs.

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

-- * Conversion functions
--
-- Functions to perform the actual provided convertions.

-- |Converts areas for the same physical size but different values for full area.
convertArea
    :: Area -- ^ old full area values
    -> Area -- ^ old desired area values
    -> Area -- ^ new full area values
    -> Area -- ^ new desired area values
convertArea (Area oldBaseOffset oldBaseDims) (Area newBaseOffset newBaseDims) (Area oldOffset oldDims)
    = let scale    = getScale oldBaseDims newBaseDims
          oldStart = subtractPairs oldOffset oldBaseOffset
      in  Area (addPairs newBaseOffset (scalePairs scale oldStart))
               (scalePairs scale oldDims)

-- |Converts areas given a physical size change.
convertBySize
    :: Pair -- ^ full size (Pair width height)
    -> Pair -- ^ desired size (Pair width height), must be in same units as the full size
    -> Area -- ^ full area values
    -> Area -- ^ desired area values
convertBySize fullSize desiredSize (Area baseOffset baseDims) =
    let scale = getScale fullSize desiredSize
    in  Area baseOffset (scalePairs scale baseDims)

-- * IO Functions
-- 
-- ** Reading input
--
-- Utilities for reading values from input.

-- |Reads a line of input with a "> " prompt.
promptInputLine :: IO String
promptInputLine = do
    putStr "> "
    hFlush stdout
    getLine

-- |Reads a list of n doubles delimited by whitespace.
getValues :: Int -> IO [Double]
getValues n = do
    inputLine <- promptInputLine
    let inputWords = words inputLine
    if length inputWords /= n
        then do
            putStrLn
                $  "Please give "
                ++ show n
                ++ " values, delimited by spaces"
            getValues n
        else
            let wordList = mapM readMaybe inputWords
            in
                case wordList of
                    Just values -> return $ take n values
                    Nothing     -> do
                        putStrLn
                            "I couldn't interpret all those values as numbers"
                        getValues n

-- |Gets a 'Pair' from input.
getSizeInput :: IO Pair
getSizeInput = do
    putStrLn "<width> <height>:"
    [width, height] <- getValues 2
    return $ Pair width height

-- |Gets an 'Area' from input.
getAreaInput :: IO Area
getAreaInput = do
    putStrLn "<left value> <top value> <right value> <bottom value>:"
    [left, top, right, bottom] <- getValues 4
    return $ Area (Pair left top) (Pair (right - left) (bottom - top))

-- ** Conversion wrappers
--
-- IO functions to run the conversion utilities

-- |Runs the area conversion utility, see 'convertArea'.
runConvertArea :: IO ()
runConvertArea = do
    putStrLn "Please enter the old full area values"
    oldFullArea <- getAreaInput
    putStrLn "Please enter the old desired area values"
    oldDesiredArea <- getAreaInput
    putStrLn "Please enter the new full area values"
    newFullArea <- getAreaInput
    let newDesiredArea = convertArea oldFullArea oldDesiredArea newFullArea
    putStrLn "I think the new desired area is:"
    print newDesiredArea

-- |Runs the size based conversion utility, see 'convertBySize'.
runConvertBySize :: IO ()
runConvertBySize = do
    putStrLn "Please enter the full size"
    fullSize <- getSizeInput
    putStrLn "Please enter the desired size"
    desiredSize <- getSizeInput
    putStrLn "Please enter the full area values"
    fullArea <- getAreaInput
    let desiredArea = convertBySize fullSize desiredSize fullArea
    putStrLn "I think the desired area is:"
    print desiredArea

-- |Represents a provided action, with strings for description.
data Command = Command { commandName :: String
                       , description :: String
                       , requirements :: [String]
                       , provisions :: [String]
                       , runCommand :: IO ()
                       }

-- |List of provided commands.
commands :: [Command]
commands =
    [ Command
        "convertArea"
        "Convert areas for the same physical size but different full area values, e.g. when changing drivers"
        [ "old full area values"
        , "old desired area values"
        , "new full area values"
        ]
        ["new desired area values"]
        runConvertArea
    , Command
        "convertBySize"
        "Get area values given the physical size of full and desired area, e.g. when changing tablet"
        ["full size", "desired size", "full area values"]
        ["desired area values"]
        runConvertBySize
    ]

-- |Displays the provided commands
displayCommands :: IO ()
displayCommands = do
    putStrLn "Supported commands:"
    putStrLn ""
    mapM_ displayCommand commands
  where
    displayCommand command = do
        putStrLn $ "  \"" ++ commandName command ++ "\":"
        putStrLn $ "    " ++ description command
        putStrLn ""
        putStrLn "    Requires:"
        mapM_ (\requirement -> putStrLn $ "    - " ++ requirement)
              (requirements command)
        putStrLn ""
        putStrLn "    Provides:"
        mapM_ (\provision -> putStrLn $ "    - " ++ provision)
              (provisions command)
        putStrLn ""

-- |Gets a command by name
getCommand :: String -> Maybe Command
getCommand name = go name commands
  where
    go n cs = if null cs
        then Nothing
        else
            let command : rest = cs
            in  if commandName command == n then Just command else go n rest
