module Main where

import Prelude

import Data.Int (quot, rem)
import Day01 as D01
import Day02 as D02
import Day03 as D03
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)

runDay :: Int -> (String -> String) -> Effect String -> Effect Unit
runDay day solution readInput =
  do
     output <- readInput <#> solution
     log ("Day " <> (quot day 10 # show) <> (rem day 10 # show) <> ": " <> output)

readFile :: String -> Effect String
readFile filename = readTextFile UTF8 filename

main :: Effect Unit
main = do
  log " == AoC 2022 == "
  runDay 1 D01.solve (readFile "input/d01.txt") 
  runDay 2 D02.solve (readFile "input/d02.txt") 
  runDay 3 D03.solve (readFile "input/d03.txt") 
