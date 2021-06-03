module Lib
    ( getWords,
      searchDiagonally,
      skew,
      searchVertically,
      searchHorizontally,
      searchAllWords,
      searchAllLines,
      reverseGrid,
      searchAtLine,
      findWord,
      outputGrid,
      randomRs,
      rows,
      columns,
      og,
      format,
      makeCoords,
      zipOverGrid,
      zipOverGridWith,
      gridWithCoords,
      findWordInCellLinePrefix,
      findWordInLine,
      line,
      findWord2,
      makeGame,
      gameGrid,
      gameWords,
      playGame,
      formatGame,
      score,
      totalWords,
      completed,
      makeRandomGrid,
      fillInBlanks,
      getLines,
      Cell (..)
    ) where

import Data.List
import Data.Maybe
import System.Random
import Data
import qualified Data.Map as M
import Data.Char (toLower)

someFunc :: IO ()
someFunc = putStrLn someString

someString :: String
someString = "someString"

type Grid a = [[a]]

data Game = Game { 
              gameGrid :: (Grid Cell),
              gameWords :: (M.Map String (Maybe [Cell]))
            } deriving Show


data Cell = Cell (Int, Int) Char | Indent deriving (Eq, Ord, Show)

makeGame :: Grid Char -> [String] -> Game
makeGame grid words =
  let gwc = gridWithCoords grid
      tuplify word = (word, Nothing)
      list = map tuplify words
      dict = M.fromList list
  in Game gwc dict

totalWords :: Game -> Int
totalWords game = length . M.keys $ gameWords game

score :: Game -> Int
score game = length . catMaybes . M.elems $ gameWords game

completed :: Game -> Bool
completed game = score game == totalWords game

playGame :: Game -> String -> Game
playGame game word | not $ M.member word (gameWords game) = game
playGame game word = 
  let grid = gameGrid game
      foundWord = findWord2 grid word
  in case foundWord of 
      Nothing -> game
      Just cs ->
        let dict = gameWords game
            newDict = M.insert word foundWord dict
        in game { gameWords = newDict}

formatGame :: Game -> String
formatGame game = formatGameGrid game
                ++ "\n\n"
                ++ (show $ score game)
                ++ "/"
                ++ (show $ totalWords game)

formatGameGrid :: Game -> String
formatGameGrid game =
  let grid = gameGrid game
      dict = gameWords game :: M.Map String (Maybe [Cell])
      cellSet = concat . catMaybes . M.elems $ dict
      formatCell cell =
        let char = cell2char cell
        in if cell `elem` cellSet then char else toLower char
      charGrid = (map . map) formatCell grid
  in unlines charGrid

makeRandomGrid gen = 
  let (gen1, gen2) = split gen
      row = randomRs ('A', 'Z') gen1
  in  row : makeRandomGrid gen2

fillInBlanks gen grid =
  let r = makeRandomGrid gen
      fill '_' r = r
      fill c _   = c
  in 
    zipOverGridWith fill grid r

------------

format grid = putStrLn (og grid)
og [] = []
og (l:ls) = show l ++ "\n" ++ (og ls)

rows l c = take l (repeat [0..(c-1)])
columns l c = transpose (rows l c)
zipOverGrid = zipWith zip
zipOverGridWith = zipWith . zipWith
makeCoords l c = zipOverGrid (columns c l) (rows l c)

gridWithCoords grid = let  n = max (length grid) (length (grid!!0))
                           coords = makeCoords n n
  in
    zipOverGridWith Cell coords grid

getWords :: [String] -> Grid Char -> [String]
getWords words grid = (searchVertically words grid) ++ (searchHorizontally words grid) ++ (searchDiagonally words grid)

searchDiagonally :: [String] -> Grid Char -> [String]
searchDiagonally words grid = let diag1 = skew grid
                                  diag2 = skew (map reverse grid)
  in
    (searchVertically words diag1) ++ (searchVertically words diag2)

skew :: Grid Char -> Grid Char
skew [] = []
skew (l:ls) = l : skew (map indent ls)
  where indent l = "_" ++ l

skew2 :: Grid Cell -> Grid Cell
skew2 [] = []
skew2 (l:ls) = l : skew2 (map indent ls)
  where indent l = [(Cell (0,0) '_')] ++ l

searchVertically :: [String] -> Grid Char -> [String]
searchVertically words grid = searchHorizontally words (transpose grid)

searchHorizontally :: [String] -> Grid Char -> [String]
searchHorizontally words grid = searchAllWords words (grid ++ (reverseGrid grid))

searchAllWords :: [String] -> Grid Char -> [String]
searchAllWords words grid = let foundWords = map (searchAllLines grid) words
  in
    catMaybes foundWords

searchAllLines :: Grid Char -> String -> Maybe String
searchAllLines grid w = let found = or $ map (findWord w) grid
  in
    if found == True then (Just w)
    else Nothing

reverseGrid :: Grid Char -> Grid Char
reverseGrid grid = map reverse grid

searchAtLine :: Grid Char -> String -> Int -> Bool
searchAtLine grid w l = findWord w (grid!!l)

findWord :: String -> String -> Bool
findWord w s = isInfixOf w s

getLines :: Grid Cell -> Grid Cell
getLines grid = grid ++ (map reverse grid) ++ (transpose grid) ++ (map reverse (transpose grid))
              ++ (transpose (reverse (skew2 (reverse grid)))) ++ (transpose (skew2 grid))
              ++ map reverse (transpose (reverse (skew2 (reverse grid))))
              ++ map reverse (transpose (skew2 grid))

findWord2 :: Grid Cell -> String -> Maybe [Cell]
findWord2 grid word =
  let lines = getLines grid
      foundWords = map (findWordInLine word) lines
  in listToMaybe (catMaybes foundWords)

findWordInLine :: String -> [Cell] -> Maybe [Cell]
findWordInLine _ [] = Nothing
findWordInLine word line =
  let found = findWordInCellLinePrefix [] word line
  in case found of
    Nothing -> findWordInLine word (tail line)
    cs@(Just _) -> cs

line = (gridWithCoords grid) !! 2

findWordInCellLinePrefix :: [Cell] -> String -> [Cell] -> Maybe [Cell]
findWordInCellLinePrefix acc (x:xs) (c:cs) | x == cell2char c
  = findWordInCellLinePrefix (c : acc) xs cs
findWordInCellLinePrefix acc [] _ = Just $ reverse acc
findWordInCellLinePrefix _ _ _ = Nothing

formatGrid :: Grid Cell -> String
formatGrid grid = 
  let charGrid = (map . map) cell2char grid
  in  unlines charGrid

cell2char :: Cell -> Char
cell2char (Cell _ c) = c

outputGrid :: Grid Cell -> IO ()
outputGrid grid = putStrLn (formatGrid grid)



