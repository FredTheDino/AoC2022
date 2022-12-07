module Day06 where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String as String
import Data.String.CodeUnits as CodeUnits

-- import Debug (traceM, spy)

findAllDiff :: Int -> Int -> String -> Maybe Int
findAllDiff _ _ "" =
  Nothing
findAllDiff q n l | (l # String.take q # CodeUnits.toCharArray # Set.fromFoldable # Set.size) == q =
  Just (n + q)
findAllDiff q n l =
  findAllDiff q (n + 1) (String.drop 1 l)

solve :: String -> String
solve input = "A) " <> (show $ findAllDiff 4 0 input) <> " B) " <> (show $ findAllDiff 14 0 input)
