module Day9 where

import Prelude

import Control.Monad.Rec.Loops (iterateUntil)
import Control.Monad.State (State, evalState, get, put)
import Data.Array.NonEmpty (NonEmptyArray, fromArray, (!!), toArray)
import Data.ArrayView as Av
import Data.ArrayView.Internal (ArrayView(..))
import Data.BigInt (BigInt)
import Data.FunctorWithIndex (mapWithIndex)
import Data.HashMap (HashMap, delete, empty, singleton, insert)
import Data.Semigroup.Foldable (maximum, minimum)
import Data.Traversable (any, foldr)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Utils (Error, bigIntParse, justToEither, lineParse, readInput, uIdxN, unsafeFromJust)

-- combs is like {fstIdx : {sndIdx : sum}}
type Combinations
  = HashMap Int (HashMap Int BigInt)

type CombState
  = State { idx :: Int, combs :: Combinations, arr :: NonEmptyArray BigInt } { idx :: Int, sumOf :: Boolean }

windowSize :: Int
windowSize = 25

step :: CombState
step = do
  st <- get
  let
    oldIdx = st.idx - windowSize

    newIdx = st.idx + 1

    newVal = uIdxN st.arr newIdx

    sumOf = any (any (_ == newVal)) st.combs

    newCombs =
      delete oldIdx st.combs
        # mapWithIndex (\k -> \h -> insert newIdx ((uIdxN st.arr k) + newVal) h)
        # insert newIdx (singleton st.idx $ (uIdxN st.arr st.idx) + newVal)
  put { idx: newIdx, combs: newCombs, arr: st.arr }
  pure { idx: newIdx, sumOf }

findNonSum :: NonEmptyArray BigInt -> BigInt
findNonSum arr =
    uIdxN arr ( _.idx
          $ evalState (iterateUntil (\x -> x.idx > windowSize && not x.sumOf) step)
              { arr, combs: empty, idx: 0 }
      )

findContSum :: BigInt -> ArrayView BigInt -> ArrayView BigInt
findContSum target av = case av of
  View { len: 0, from, arr } -> go $ View { from: from + 1, len: 1, arr }
  View { len, from, arr } -> case compare (foldr (+) zero av) target of
    LT -> go $ View { from, len: len + 1, arr }
    GT -> go $ View { from: from + 1, len: len - 1, arr }
    EQ -> av
  where go = findContSum target

parse :: String -> Error (NonEmptyArray BigInt)
parse s = do
  arr <- lineParse bigIntParse s
  justToEither "array empty!" $ fromArray arr

main :: Effect Unit
main = do
  input <- readInput 9
  log $ show
    $ do
        parsed <- parse input
        let
          bad = findNonSum parsed
        contSum <-
          justToEither "empty"
            $ fromArray
            $ Av.toArray
            $ findContSum bad
            $ View { arr: (toArray parsed), len: 1, from: 0 }
        let
          smallest = minimum contSum
        let
          largest = maximum contSum
        pure $ Tuple bad (smallest + largest)
