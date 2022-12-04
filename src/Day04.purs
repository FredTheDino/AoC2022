module Day04 where

import Prelude

import Data.Array ((!!))
import Data.Array as Array
import Data.Int (fromString)
import Data.Maybe (Maybe)
import Data.String (Pattern(..))
import Data.String as String

-- import Debug (traceM, spy)

type Range = { lo :: Int, hi :: Int }

parseLine :: String -> Maybe { a :: Range, b :: Range }
parseLine line =
  let
    xs =
      line
        # String.split (Pattern ",")
        # Array.concatMap (String.split (Pattern "-"))
  in
  ado
    a <- xs !! 0 >>= fromString
    b <- xs !! 1 >>= fromString
    c <- xs !! 2 >>= fromString
    d <- xs !! 3 >>= fromString
    in { a: { lo: a, hi: b }, b: { lo: c, hi: d } }

containsSuperSet :: { a :: Range, b :: Range } -> Boolean
containsSuperSet { a, b } =
  (a.lo <= b.lo && b.hi <= a.hi)
    || (b.lo <= a.lo && a.hi <= b.hi)

anyOverlap :: { a :: Range, b :: Range } -> Boolean
anyOverlap { a, b } =
  (a.lo <= b.lo && b.lo <= a.hi)
    || (a.lo <= b.hi && b.hi <= a.hi)
    || (b.lo <= a.lo && a.lo <= b.hi)
    || (b.lo <= a.hi && a.hi <= b.hi)

solve :: String -> String
solve input =
  let
    lines =
      input
        # String.split (Pattern "\n")
        # Array.mapMaybe parseLine

    a = lines # Array.filter containsSuperSet # Array.length
    b = lines # Array.filter anyOverlap # Array.length
  in
  "A) " <> show a <> " B) " <> show b
