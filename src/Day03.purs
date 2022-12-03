module Day03 where

import Prelude

import Data.Array as Array
import Data.Enum (class BoundedEnum, fromEnum)
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))

-- import Debug (traceM, spy)

prios :: Array Int
prios = [-1] <> Array.range 97 122 <> Array.range 65 90

scoreItem :: forall a. BoundedEnum a => a -> Maybe Int
scoreItem x = fromEnum x `Array.elemIndex` prios

findDoubleItem :: String -> Array Int
findDoubleItem line =
    let
      { before, after } = String.splitAt (String.length line / 2) line
      before' = before # String.toCodePointArray # Set.fromFoldable 
      kept = after # String.toCodePointArray # Array.filter (\x -> x `Set.member` before')  # Array.nub
    in
    kept # Array.mapMaybe scoreItem

findUniqueItem :: _ -> Maybe Int
findUniqueItem [a, b, c] =
    let
      toSet s = s # String.toCodePointArray # Set.fromFoldable
    in
    (toSet a) `Set.intersection` (toSet b) `Set.intersection` (toSet c) # Set.findMin >>= scoreItem
findUniqueItem _ = Nothing

buildGroups :: Array String -> Array (Array String)
buildGroups [] = []
buildGroups x = [Array.take 3 x] <> buildGroups (Array.drop 3 x)

solve :: String -> String
solve input =
  let
    lines = input # String.split (Pattern "\n")
    a = lines # map findDoubleItem # map sum # sum
    b = lines # buildGroups # Array.mapMaybe findUniqueItem # sum
  in
  "A) " <> show a <> " B) " <> show b
