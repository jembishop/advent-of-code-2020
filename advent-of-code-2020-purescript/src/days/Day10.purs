module Day10 where

import Prelude

import Data.Array (cons, filter, length, span, zipWith)
import Data.Array.NonEmpty (NonEmptyArray, fromArray, toArray, uncons, (:))
import Data.Array.NonEmpty as Na
import Data.BigInt (BigInt)
import Data.Semigroup.Foldable (maximum)
import Data.Traversable (foldr)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Utils (Error, justToEither, lineParse, numParse, readInput, diff)


parse :: String -> Error (NonEmptyArray Int)
parse s = do
  arr <- lineParse numParse s
  narr <- justToEither "array empty!" $ fromArray arr
  pure $ Na.sort (0: (maximum narr + 3) : narr)


part1 :: NonEmptyArray Int -> Int
part1 arr = (numX 1) * (numX 3)
    where 
       numX x = length $ filter (_==x) (diff arr)

oneIslands :: Array Int -> Array Int 
oneIslands arr = if arr == [] then [] else cons (length init) (oneIslands filRem)
    where 
        {init, rest: rem} = span (_==1) arr
        {init: _, rest: filRem} = span (_==3) rem

oneIslandCombs :: Int -> BigInt
oneIslandCombs i = case i of 
    0 -> one
    1 -> one
    2 -> one + one
    n -> (oneIslandCombs (n - 1)) + (oneIslandCombs (n - 2)) + (oneIslandCombs (n - 3))

part2 :: NonEmptyArray Int -> BigInt
part2 arr =  foldr (*) one $ map oneIslandCombs $ oneIslands (diff arr)

main :: Effect Unit
main = do
  input <- readInput 10
  log $ show
    $ do
        parsed <- parse input
        let p1 = part1 parsed
        let p2 = part2 parsed
        pure $ Tuple p1 p2