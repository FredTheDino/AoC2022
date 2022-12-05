module Day05 where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Int (fromString)
import Data.List as List
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..))
import Data.String (drop, joinWith, split, take) as String
import Data.String.Utils (startsWith) as String
-- import Debug (traceM, spy)

type Crate = String

parseLine :: String -> Array (Maybe String)
parseLine line = Array.range 0 9 <#> a
  where
  a i =
    case line # String.drop (1 + 4 * i) # String.take 1 of
      "" -> Nothing
      " " -> Nothing
      c -> Just c

parseStacks :: Array String -> Array (Array String)
parseStacks lines =
  do
    lines
      # map parseLine
      # map List.fromFoldable
      # List.fromFoldable
      # List.transpose
      # Array.fromFoldable
      # map Array.fromFoldable
      # map Array.catMaybes

parseMove :: String -> Maybe { nr :: Int, from :: Int, to :: Int }
parseMove line =
  let
    split = String.split (Pattern " ") line
  in
  ado
    nr <- split !! 1 >>= fromString
    from <- split !! 3 >>= fromString <#> \a -> a - 1
    to <- split !! 5 >>= fromString <#> \a -> a - 1
    in { nr, from, to }

-- An ugly hack to go around the partial constraint, since this should never fail
fromJust' :: forall a. Maybe a -> a
fromJust' a = case  a of
              Just x -> x
              Nothing -> fromJust' a

simulateA :: { nr :: Int, from :: Int, to :: Int } -> Array (Array String) -> Array (Array String)
simulateA { nr, from, to } state =
  state
    # Array.modifyAt to (\x -> x <> moved)
    # fromJust'
    # Array.modifyAt from (Array.dropEnd nr)
    # fromJust'
  where
    moved = state !! from <#> Array.takeEnd nr # fromJust'

simulateB :: { nr :: Int, from :: Int, to :: Int } -> Array (Array String) -> Array (Array String)
simulateB { nr, from, to } state =
  state
    # Array.modifyAt to (\x -> x <> moved)
    # fromJust'
    # Array.modifyAt from (Array.dropEnd nr)
    # fromJust'
  where
    moved = state !! from <#> Array.takeEnd nr # fromJust' # Array.reverse 


solve :: String -> String
solve input =
  let
    lines =
      input
        # String.split (Pattern "\n")

    initalState =
      lines
        # Array.takeWhile (String.startsWith "move" >>> not)
        # Array.dropEnd 2
        # Array.reverse
        # parseStacks

    moves =
      lines
        # Array.dropWhile (String.startsWith "move" >>> not)
        # Array.mapMaybe parseMove
        # Array.reverse

    a = moves 
            # Array.foldr simulateA initalState 
            # map (Array.last >>> fromMaybe " ")
            # String.joinWith ""

    b = moves 
            # Array.foldr simulateB initalState 
            # map (Array.last >>> fromMaybe " ")
            # String.joinWith ""
  in
  "A) " <> show a <> " B) " <> show b
