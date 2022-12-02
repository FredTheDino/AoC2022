module Day02 where

import Prelude

import Data.Array as Array
import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.Tuple (Tuple(..))
-- import Debug (traceM, spy)

data RPS = R | P | S

derive instance Eq RPS

data TheirMove = A | B | C

toTheirMove :: String -> Maybe TheirMove
toTheirMove a =
  case a of
    "A" -> Just A
    "B" -> Just B
    "C" -> Just C
    _ -> Nothing

data MyMove = Y | X | Z

toMyMove :: String -> Maybe MyMove
toMyMove a =
  case a of
    "X" -> Just X
    "Y" -> Just Y
    "Z" -> Just Z
    _ -> Nothing

parse :: String -> Array { myMove :: MyMove, theirMove :: TheirMove }
parse a =
  a
    # String.split (Pattern "\n")
    # Array.mapMaybe parseLine
  where
  parseLine line =
    ado
      theirMove <- line # String.take 1 # toTheirMove
      myMove <- line # String.drop 2 # toMyMove
      in { theirMove, myMove }

myToRPS :: MyMove -> RPS
myToRPS a =
  case a of
    X -> R
    Y -> P
    Z -> S

theirToRPS :: TheirMove -> RPS
theirToRPS a =
  case a of
    A -> R
    B -> P
    C -> S

scorePartOne :: { myMove :: MyMove, theirMove :: TheirMove } -> Int
scorePartOne { myMove, theirMove } = moveScore + wonScore
  where
  wonScore =
    case myToRPS myMove, theirToRPS theirMove of
      a, b | a == b -> 3
      R, S -> 6
      S, P -> 6
      P, R -> 6
      _, _ -> 0

  moveScore =
    case myToRPS myMove of
      R -> 1
      P -> 2
      S -> 3

scorePartTwo :: { myMove :: MyMove, theirMove :: TheirMove } -> Int
scorePartTwo { myMove, theirMove } = moveScore + wonScore
  where
  Tuple wonScore move =
    case myMove, theirToRPS theirMove of
      X, R -> Tuple 0 S
      X, P -> Tuple 0 R
      X, S -> Tuple 0 P
      --
      Y, R -> Tuple 3 R
      Y, P -> Tuple 3 P
      Y, S -> Tuple 3 S
      --
      Z, S -> Tuple 6 R
      Z, R -> Tuple 6 P
      Z, P -> Tuple 6 S

  moveScore =
    case move of
      R -> 1
      P -> 2
      S -> 3

solve :: String -> String
solve input =
  let
    totalScoreOne = input # parse # map scorePartOne # sum
    totalScoreTwo = input # parse # map scorePartTwo # sum
  in
  "A) " <> show totalScoreOne <> " B) " <> show totalScoreTwo
