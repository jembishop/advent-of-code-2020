module Day7 where

import Prelude
import Data.Array (elem, filter, index, length)
import Data.Either (Either)
import Data.Foldable (foldr)
import Data.HashMap (HashMap, empty, insert, lookup)
import Data.Int (fromString)
import Data.String (Pattern(..), split)
import Data.String.Utils (lines)
import Data.Traversable (any)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Utils (justToEither, tryArr, unsafeFromJust)

type BagRule
  = (Tuple String (Array (Tuple String Int)))

parseColEntry :: String -> Either String (Tuple String Int)
parseColEntry s = do
  let
    sp = split (Pattern " ") s
  cn <- justToEither "col entry num" $ index sp 0
  cnp <- justToEither "num parse" $ fromString cn
  cf <- justToEither "col entry fst half" $ index sp 1
  cs <- justToEither "col entry snd half" $ index sp 2
  pure (Tuple (cf <> " " <> cs) cnp)

preParse :: String -> Either String BagRule
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
  ce <- tryArr parseColEntry sp2s
  pure (Tuple (fh <> " " <> sh) ce)

parse :: String -> Either String (Array BagRule)
parse s = tryArr preParse (filter (\x -> x /= "") (lines s))

canContain :: String -> String -> HashMap String BagRule -> Boolean
canContain target bag allBags = case unsafeFromJust (lookup bag allBags) of
  (Tuple _ []) -> false
  (Tuple _ l) ->
    if elem target bagCols then
      true
    else
      any identity $ map (\x -> canContain target x allBags) bagCols
    where
    bagCols = map fst l

numContain :: String -> HashMap String BagRule -> Int
numContain bag allBags = case unsafeFromJust (lookup bag allBags) of
  (Tuple _ []) -> 0
  (Tuple _ l) -> foldr (+) 0 (map (\(Tuple s i) -> i + i * (numContain s allBags)) l)

makeBagRuleMap :: Array BagRule -> HashMap String BagRule
makeBagRuleMap arr = foldr (\y -> \x -> insert (fst y) y x) empty arr

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "inputs/day7.txt"
  log $ show
    $ do
        parsed <- parse input
        let
          m = makeBagRuleMap parsed
        let
          p1 = length $ filter identity $ map (\b -> canContain "shiny gold" b m) (map fst parsed)
        let
          p2 = numContain "shiny gold" m
        pure (Tuple p1 p2)
