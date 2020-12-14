module Day13 where

import Debug.Trace
import Prelude

import Control.Alternative ((<|>))
import Control.Apply (lift2)
import Data.Array.NonEmpty (NonEmptyArray, catMaybes, fromArray, head, index, uncons)
import Data.BigInt (BigInt, fromInt)
import Data.Either (Either(..))
import Data.Foldable (foldr, maximum, minimum, minimumBy)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), isJust)
import Data.Ord (abs)
import Data.String (Pattern(..), split, splitAt)
import Data.Traversable (any, sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, clear)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)
import Utils (Error, errThrow, justToEither, lineParse, numParse, readFile, readInput, unsafeFromJust)


parseBus :: String -> Error (Maybe Int)
parseBus s = isX s <|> (Just <$> (numParse s))
      where
        isX c = if c == "x" then Right Nothing else Left "not x"

type Schedule = {timestamp:: Int, buses::NonEmptyArray (Maybe Int)}

parse :: String -> Error Schedule
parse s = do
    sp <- justToEither "nonempty" $ fromArray $ split (Pattern "\n") s
    fh <- justToEither "fl" $ index sp 0
    timestamp <- numParse fh
    sh <- justToEither "sl" $ index sp 1
    bs <- justToEither "buses" $ fromArray $ split (Pattern ",") sh
    buses <- sequence $ map parseBus $ bs
    pure {timestamp, buses}

part1 :: Schedule -> Int
part1 s = (time min_id) * min_id
    where
        min_id = unsafeFromJust $ minimumBy (comparing time) $ (catMaybes s.buses)
        time id = id - (s.timestamp `mod` id)

-- given a b returns x y : Z such that ax + by = 1 
extendedEuclideanH :: {r0::BigInt, r1::BigInt, 
                       s0::BigInt, s1::BigInt, 
                       t0 ::BigInt, t1 :: BigInt} -> {x::BigInt, y::BigInt}
extendedEuclideanH im = if im.r1 == zero then {x:im.s0, y:im.t0} else 
    let q = im.r0 / im.r1 in
     extendedEuclideanH {
        r0 : im.r1,
        s0 : im.s1,
        t0 : im.t1,
        r1 : im.r0 - q*im.r1,
        s1 : im.s0 - q*im.s1,
        t1 : im.t0 - q*im.t1
    }

extendedEuclidean :: {a :: BigInt, b::BigInt} -> {x::BigInt, y:: BigInt}
extendedEuclidean im = extendedEuclideanH {
    r0: im.a, r1: im.b, s0: one, s1: zero , t0: zero,  t1:one}

-- given t mod p and t mod q, p q rel prime we can compute t mod pq by chinese remainder theorem.
-- we can keep repeating this as all the nums are distinct rel primes 
-- as timestamp is minimum and +ve we can add on product till we get to first +ve
modPQ :: {r:: BigInt, p:: BigInt}-> {r:: BigInt , p:: BigInt}-> {r:: BigInt, p:: BigInt}
modPQ {r, p} {r: s, p: q} =  {r: s*a*p + r*b*q, p: p*q}
    where 
        {x:a, y:b} = extendedEuclidean {a: p, b: q}


part2 :: Schedule -> BigInt
part2 s = modProd.r `mod` modProd.p
    where
        xs = unsafeFromJust $ fromArray $ catMaybes $ mapWithIndex (\i -> \x-> (\y -> {r: fromInt ((y-i)`mod`y), p: fromInt y}) <$> x) s.buses
        {head, tail} = uncons xs
        modProd = foldr modPQ head tail  

main :: Effect Unit
main = do
  input <- readInput 13
  parsed <- errThrow $ parse input 
  log $ show $ part1 parsed
  log $ show $ part2 parsed