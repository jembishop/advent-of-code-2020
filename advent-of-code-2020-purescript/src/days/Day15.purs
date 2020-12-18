
module Day15 where

import Data.HashSet
import Data.Long
import Data.Long.Bits
import Debug.Trace
import Prelude

import Control.Alternative ((<|>))
import Control.Apply (lift2)
import Data.Array (filter, sort)
import Data.Array.NonEmpty (length, range, unsnoc)
import Data.Array.NonEmpty as NE
import Data.Either (Either(..))
import Data.Foldable (foldr, maximum, minimum, minimumBy, foldl)
import Data.Functor (map)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HashMap (HashMap, fromFoldable, insert, lookup, toArrayBy)
import Data.Int (binary)
import Data.Long (fromInt, Long)
import Data.Long.Internal (fromStringAs, toStringAs)
import Data.Maybe (Maybe(..), isJust)
import Data.Ord (abs)
import Data.String (Pattern(..), split, splitAt)
import Data.String.CodeUnits (fromCharArray, singleton, toCharArray)
import Data.String.Utils (stripChars)
import Data.Traversable (any, sequence, sum)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, clear)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)
import Utils (Error, errThrow, justToEither, lineParse, numParse, readFile, readInput, unsafeFromJust, unsafeFromRight)

parse :: String -> Error (NE.NonEmptyArray Int)
parse s = do 
    sp <- justToEither "nonempty" $ NE.fromArray $ split (Pattern ",") (stripChars "\n" s)
    nums <- sequence (map numParse sp)
    pure nums

type Info = {spoken:: HashMap Int Int, lastSpoken::Int}

step :: Info -> Int -> Info
step info tn = {spoken: insert info.lastSpoken (tn - 1) info.spoken, lastSpoken: new}
    where 
        -- arrg1 = unsafePerformEffect (log $ show info.lastSpoken)
        new = case lookup info.lastSpoken info.spoken of 
            Nothing -> 0
            Just i -> tn - i - 1

        -- arrg2 = unsafePerformEffect 
        --     (log $ "TURN NUMBER  " <> show tn <> " new " <> show new)
        -- arrg3 = unsafePerformEffect 
        --     (log $ show $ sort $ toArrayBy (\x -> \y -> (Tuple x y)) info.spoken)
        -- arrg3 = unsafePerformEffect (log $ show info.spoken)


part1 :: Int -> NE.NonEmptyArray Int -> Int
part1 n arr = _.lastSpoken $
     foldl step initial (range (length arr + 1) n ) 
        where 
            {init, last} = unsnoc arr
            initial = {spoken: fromFoldable $ mapWithIndex (\i -> \x -> Tuple x (i + 1)) init, lastSpoken: last} 
            -- arrg2 = unsafePerformEffect (log $ "INITIAL " <> (show initial))

main :: Effect Unit
main = do
  input <- readInput 15
--   input <- readFile "inputs/test.txt"
  parsed <- errThrow $ parse input 
  log $ show $ part1 300 parsed
  log $ show $ part1 30000000 parsed
  log $ show $ part1 2020 parsed