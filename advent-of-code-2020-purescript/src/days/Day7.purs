module Day7 where

import Data.Either
import Debug.Trace
import Prelude

import Control.MonadZero (guard)
import Data.Array (catMaybes, cons, elem, filter, index, length, uncons, (:))
import Data.Array.NonEmpty (concatMap, tail)
import Data.Foldable (foldr)
import Data.Functor.Product.Nested (product2)
import Data.HashMap (HashMap, empty, insert, lookup)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Data.String (Pattern(..), split)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (noFlags)
import Data.String.Utils (lines)
import Data.Traversable (any, find, sequence)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Stream (onFinish)
import Partial.Unsafe (unsafePartial)
import Prim.Boolean (False)



maybesToEithers :: forall a. String -> Array (Maybe a) -> Either String (Array a)
maybesToEithers s ls = go 0 ls
  where 
    go :: Int -> Array (Maybe a) -> Either String (Array a)
    go n l = case uncons l of 
      Just {head: Just x, tail: xs} -> (\y -> cons x y) <$> go (n + 1) xs
      Just {head: Nothing, tail: _} -> Left ("Line number " <> (show n) <> " failed to parse in " <> s) 
      Nothing -> Right []

justToEither :: forall a. String -> Maybe a -> Either String a
justToEither s ma = case ma of
      (Just r)  -> Right r
      Nothing  -> Left ("Error " <> s)

toPairs ::forall a . Array a -> Maybe (Array (Tuple a a))
toPairs arr = case arr of
  [] -> Just []
  a -> do 
    {head: x, tail: xs} <- uncons a
    {head: y, tail: ys} <- uncons xs
    rest <- toPairs ys
    pure ((Tuple x y) : rest)


type BagRule = (Tuple String (Array (Tuple String Int)))

parseColEntry :: String -> Either String (Tuple String Int)
parseColEntry s = do  
  let sp = split (Pattern " ") s
  cn <- justToEither "col entry num" $ index sp 0
  cnp <- justToEither "num parse" $ fromString cn
  cf <- justToEither "col entry fst half" $ index sp 1
  cs <- justToEither "col entry snd half" $ index sp 2
  pure (Tuple (cf <> " " <> cs) cnp)

tryArr :: forall a. (String -> Either String a) -> Array String -> Either String (Array a)
tryArr p s = go 0 p s 
  where 
    go n p s = case uncons s of 
      Just {head: h,  tail: t} -> case p h of 
        Right x -> map (cons x) (go (n+1) p t)
        Left s -> Left ("Error at #" <> (show n) <> ": " <> s)
      Nothing -> Right []

preParse :: String -> Either String BagRule
preParse s = do
  let sp = split (Pattern " contain ") s
  sp2 <- justToEither "fst half" $ index sp 0
  let sp2f = split (Pattern " ") sp2
  fh <- justToEither "col fst half" $ index sp2f 0
  sh <- justToEither "col snd half" $ index sp2f 1
  spl <- justToEither "snd half" $ index sp 1
  let sp2s = if spl == "no other bags." then [] else split (Pattern ", ") spl
  ce <- tryArr parseColEntry sp2s
  pure (Tuple (fh <> " " <> sh) ce)


parse :: String -> Either String (Array BagRule)
parse s = tryArr preParse (filter (\x -> x/="") (lines s))

unsafeFromJust = unsafePartial fromJust
canContain :: String -> HashMap String BagRule -> BagRule -> Boolean
canContain bag allBags br = case br of 
  (Tuple _ []) -> false 
  (Tuple _ l) -> if elem bag bagCols then true else any identity $ map (canContain bag allBags) matches
      where 
       bagCols = map fst l 
       matches :: Array BagRule
       matches = unsafeFromJust $ sequence $ map (\s -> lookup s allBags) bagCols


numContain :: String -> HashMap String BagRule -> Int
numContain bag allBags = case unsafeFromJust (lookup bag allBags) of 
  (Tuple _ []) -> 0
  (Tuple _ l) -> foldr (+) 0 (map (\(Tuple s i) -> i + i*(numContain s allBags)) l)

makeBagRuleMap :: Array BagRule -> HashMap String BagRule
makeBagRuleMap arr = foldr (\y -> \x -> insert (fst y) y x) empty arr

main :: Effect Unit
main = do
  input <- readTextFile UTF8 "inputs/day7.txt"
  log $ show $ do 
    parsed <- parse input
    let m = makeBagRuleMap parsed
    let p1 = length $ filter identity $ map (canContain "shiny gold" m) parsed
    let p2 = numContain "shiny gold" m 
    pure (Tuple p1 p2)

