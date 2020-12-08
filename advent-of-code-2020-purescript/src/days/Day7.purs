module Day7 where

import Prelude

import Data.Array (elem, filter, index, length)
import Data.Either (Either)
import Data.Foldable (foldr)
import Data.HashMap (HashMap, empty, insert, lookup)
import Data.String (Pattern(..), split)
import Data.Traversable (any, traverse)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Console (log)
import Utils (Error, justToEither, lineParse, numParse, readInput, unsafeFromJust)

type BagRule
  = (Tuple String (Array (Tuple String Int)))

parseColEntry :: String -> Error (Tuple String Int)
parseColEntry s = do
  let
    sp = split (Pattern " ") s
  cn <- justToEither "col entry num" $ index sp 0
  cnp <- numParse cn
  cf <- justToEither "col entry fst half" $ index sp 1
  cs <- justToEither "col entry snd half" $ index sp 2
  pure $ Tuple (cf <> " " <> cs) cnp

preParse :: String -> Error BagRule
preParse s = do
  let
    sp = split (Pattern " contain ") s
  sp2 <- justToEither "fst half" $ index sp 0
  let
    sp2f = split (Pattern " ") sp2
  fh <- justToEither "col fst half" $ index sp2f 0
  sh <- justToEither "col snd half" $ index sp2f 1
  spl <- justToEither "snd half" $ index sp 1
  let
    sp2s = if spl == "no other bags." then [] else split (Pattern ", ") spl
  ce <- traverse parseColEntry sp2s
  pure $ Tuple (fh <> " " <> sh) ce

parse :: String -> Error (Array BagRule)
parse s = lineParse preParse s 

canContain :: String -> String -> HashMap String BagRule -> Boolean
canContain target bag allBags = elem target bagCols || (any identity $ map (\x -> canContain target x allBags) bagCols)
  where
  (Tuple _ bs) = unsafeFromJust (lookup bag allBags)

  bagCols = map fst bs

numContain :: String -> HashMap String BagRule -> Int
numContain bag allBags = foldr (+) 0 $ map (\(Tuple s i) -> i + i * (numContain s allBags)) bs
  where
  (Tuple _ bs) = unsafeFromJust (lookup bag allBags)

makeBagRuleMap :: Array BagRule -> HashMap String BagRule
makeBagRuleMap arr = foldr (\y -> \x -> insert (fst y) y x) empty arr

main :: Effect Unit
main = do
  input <- readInput 7
  log $ show
    $ do
        parsed <- parse input
        let
          m = makeBagRuleMap parsed

          chosenBag = "shiny gold"

          p1 = length $ filter identity $ map (\b -> canContain chosenBag b m) (map fst parsed)

          p2 = numContain chosenBag m
        pure $ Tuple p1 p2
