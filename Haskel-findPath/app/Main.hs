module Main where

import Lib
import System.IO
import Data.Char
import Numeric (showHex, showIntAtBase, readHex)
import Data.List
import Text.Printf (printf)
import Control.Exception
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  if (length args) /= 2
    then putStrLn "ArgumentError: There needs to be two arguments, inputPath and outputPath."
    else do
      result <- try (findPath (args !! 0) (args !! 1))   :: IO (Either SomeException ())
      case result of
        Left ex -> putStrLn "InputError: InputFile cannot be found / opened."
        Right result -> putStr ""


dec :: String -> [Int]
dec [] = []
dec (x:xs) = ord(x):dec xs

hex :: Int -> String
hex a = if a < 16
  then
    "0" ++ (showHex a "")
  else
    showHex a ""

getCharsContents :: Handle -> IO [Char]
getCharsContents fileHandle = do
  isEofFile <- hIsEOF fileHandle
  if isEofFile
     then return []
     else do
       info <- hGetLine fileHandle
       remain <- getCharsContents fileHandle
       return (info ++ "\n" ++ remain)

getFileContents :: Handle -> IO [Int]
getFileContents fileHandle = do
  info <- getCharsContents fileHandle
  return (dec info)

getIntList :: String -> IO [Int]
getIntList fileName = do
  fileHandle <- openBinaryFile fileName ReadMode
  intList <- getFileContents fileHandle
  hClose fileHandle
  return intList

separateMetaMaze :: [Int] -> ([Int], [Int], [Int])
separateMetaMaze x = let (f, s) = splitAt 8 x in
  let (f1, f2) = splitAt 4 f in (reverse f1, reverse f2, s)

hexChar :: Char -> Maybe Int
hexChar ch = elemIndex ch "0123456789abcdef"

parseHex :: String -> Int -> Int
parseHex [] _ = 0
parseHex (x:xs) c = do
  let v = getValueMaybe $ hexChar x
  v * 16^c + (parseHex xs (c-1))

getValueMaybe :: Maybe Int -> Int
getValueMaybe (Just x) = x
getValueMaybe Nothing = -1

hexToBin :: Char -> String
hexToBin c = case readHex [c] of (x,_):_ -> printf "%04b" (x::Int)

splitAfter8Reverse :: String -> [String]
splitAfter8Reverse str = if (length str > 8)
  then let (f, s) = splitAt 8 str in [reverse f] ++ (splitAfter8Reverse s)
  else [reverse str]

type Position = (Int, Int)
type Size = (Int, Int)
type Below = Bool
type Right = Bool
type CellWall = (Below, Right)
type Walls = [CellWall]
type Maze = (Size, Walls)

createWalls :: String -> Walls
createWalls "" = []
createWalls (x1:x2:xs)
  | x1 == '1' && x2 == '1' = [(True, True)] ++ (createWalls xs)
  | x1 == '1' && x2 == '0' = [(False, True)] ++ (createWalls xs)
  | x1 == '0' && x2 == '1' = [(True, False)] ++ (createWalls xs)
  | x1 == '0' && x2 == '0' = [(False, False)] ++ (createWalls xs)

readBinaryFile :: String -> IO Maze
readBinaryFile fileName = do
  ints <- getIntList fileName
  let (h, w, m) = separateMetaMaze ints
  let width = parseHex (concat $ map hex w) 7
  let height = parseHex (concat $ map hex h) 7
  let mazeInfo = concat $ splitAfter8Reverse $ concat $ map hexToBin (concat $ map hex m)
  let size = (height, width)
  let (walls, _) = splitAt (width * height) (createWalls mazeInfo)
  return (size, walls)

getWallsIndex :: Size -> Int -> Int -> Int
getWallsIndex (_, w) r c = r*w + c

placeWall :: Maze -> CellWall -> Int -> Int -> Walls
placeWall (s, w) cw r c = do
  let splitIndex = (getWallsIndex s r c) in
    let (w1, (x:w2)) = splitAt splitIndex w in (w1 ++ [cw] ++ w2)

data LastMvmt = East | South | West | North deriving (Eq, Show)
type Movement = (Position, LastMvmt)
type Path = [Movement]

canTurnEast :: Maze -> Movement -> Bool
canTurnEast ((h, w), walls) ((r, c), mvmt)
  | c == (w-1) || mvmt == West = False
  | otherwise = let (_, right) = walls !! (getWallsIndex (h, w) r c) in not right

canTurnSouth :: Maze -> Movement -> Bool
canTurnSouth ((h, w), walls) ((r, c), mvmt)
  | r == (h-1) || mvmt == North = False
  | otherwise = let (below, _) = walls !! (getWallsIndex (h, w) r c) in not below

canTurnWest :: Maze -> Movement -> Bool
canTurnWest ((h, w), walls) ((r, c), mvmt)
  | c == 0 || mvmt == East = False
  | otherwise = let (_, right) = walls !! (getWallsIndex (h, w) r (c-1)) in not right

canTurnNorth :: Maze -> Movement -> Bool
canTurnNorth ((h, w), walls) ((r, c), mvmt)
  | r == 0 || mvmt == South = False
  | otherwise = let (below, _) = walls !! (getWallsIndex (h, w) (r-1) c) in not below

findPathHelper :: Maze -> Path -> Movement -> IO Path
findPathHelper ((h, w), walls) p ((lr, lc), lmv)
  | lr == (h-1) && lc == (w-1) = return p
  | canTurnEast ((h, w), walls) ((lr, lc), lmv) = findPathHelper ((h, w), walls) (p ++ [((lr, lc+1), East)]) ((lr, lc+1), East)
  | canTurnSouth ((h, w), walls) ((lr, lc), lmv) = findPathHelper ((h, w), walls) (p ++ [((lr+1, lc), South)]) ((lr+1, lc), South)
  | canTurnWest ((h, w), walls) ((lr, lc), lmv) = findPathHelper ((h, w), walls) (p ++ [((lr, lc-1), West)]) ((lr, lc-1), West)
  | canTurnNorth ((h, w), walls) ((lr, lc), lmv) = findPathHelper ((h, w), walls) (p ++ [((lr-1, lc), North)]) ((lr-1, lc), North)
  | lr == 0 && lc == 0 = return p
  | lmv == East = do
    let newPath = init p
    let ((llr, llc), llmvt) = last newPath
    let (below, _) = walls !! (getWallsIndex (h, w) llr llc)
    let newWalls = placeWall ((h, w), walls) (below, True) llr llc
    let newMaze = ((h, w), newWalls)
    findPathHelper newMaze newPath ((llr, llc), llmvt)
  | lmv == South = do
    let newPath = init p
    let ((llr, llc), llmvt) = last newPath
    let (_, right) = walls !! (getWallsIndex (h, w) llr llc)
    let newWalls = placeWall ((h, w), walls) (True, right) llr llc
    let newMaze = ((h, w), newWalls)
    findPathHelper newMaze newPath ((llr, llc), llmvt)
  | lmv == West = do
    let newPath = init p
    let (below, _) = walls !! (getWallsIndex (h, w) lr lc)
    let newWalls = placeWall ((h, w), walls) (below, True) lr lc
    let newMaze = ((h, w), newWalls)
    findPathHelper newMaze newPath (last newPath)
  | lmv == North = do
    let newPath = init p
    let (_, right) = walls !! (getWallsIndex (h, w) lr lc)
    let newWalls = placeWall ((h, w), walls) (True, right) lr lc
    let newMaze = ((h, w), newWalls)
    findPathHelper newMaze newPath (last newPath)

createStringForOutput :: Path -> String
createStringForOutput [] = ""
createStringForOutput (((r, c), _):xs) = (show r) ++ " " ++ (show c) ++ "\n" ++ (createStringForOutput xs)

findPath :: String -> String -> IO ()
findPath inputName outputName = do
  maze <- (readBinaryFile inputName)
  let initMvmt = ((0, 0), East)
  let ((h, w), walls) = maze
  path <- try (findPathHelper maze [initMvmt] initMvmt) :: IO (Either SomeException Path)
  case path of
    Left ex -> putStrLn "MazeError: The provided input file is not a valid maze."
    Right path -> do
                    if (length path) == 1 && (h /= 1 || w /= 1)
                      then putStrLn "MazeError: There is no path to the exit."
                      else do
                            let outputString = createStringForOutput path
                            result <- try (writeFile outputName outputString) :: IO (Either SomeException ())
                            case result of
                              Left ex -> putStrLn "WriteError: Cannot write to the provided output file."
                              Right result -> putStrLn "Program executed successfully !"
