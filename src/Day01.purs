module Day01 where

import Prelude

import Data.Maybe (fromMaybe)
import Data.Array (foldr)
import Data.Array.NonEmpty as NEA
import Data.Foldable (maximum, sum)
import Data.Int (fromString)
import Data.String (split, Pattern(..))
import Data.Traversable (traverse)
-- import Debug (traceM)

group :: Array String -> NEA.NonEmptyArray (Array String)
group = foldr combine (NEA.singleton [])
  where
  combine :: String -> NEA.NonEmptyArray (Array String) -> NEA.NonEmptyArray (Array String)
  combine a curr =
    if a == "" then
      NEA.cons [] curr
    else
      NEA.cons' ([ a ] <> head) tail
    where
    { tail, head } = NEA.uncons curr

solve :: String -> String
solve input =
  let
    perElv =
      input
        # split (Pattern "\n")
        # group
        # traverse (traverse fromString)
        # fromMaybe (NEA.singleton [])
        # map sum

    largest = perElv # maximum # fromMaybe (-1)

    top3Summ = perElv # NEA.sort # NEA.takeEnd 3 # sum
  in
  ("A) " <> show largest <> " B) " <> show top3Summ)
