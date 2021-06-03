module Main where

import Lib
import Data
import System.IO
import System.Random

main :: IO ()
main = do
   gen <- newStdGen
   let filledInGrid = fillInBlanks gen grid
       game = makeGame filledInGrid languages
   hSetBuffering stdout NoBuffering
   playTurn game

playTurn game = do
   putStrLn . formatGame $ game
   putStrLn "Please enter a word> "
   word <- getLine
   let newGame = playGame game word
   if completed newGame then
      putStrLn "Congradulations!"
   else
      playTurn newGame
