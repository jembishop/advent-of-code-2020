module Utils where

import Prelude

import Data.Array (uncons, (:), filter)
import Data.Bifunctor (lmap)
import Data.Either (Either(..))
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromJust)
import Data.String.Utils (lines)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafePartial)

justToEither :: forall a. String -> Maybe a -> Either String a
justToEither s ma = case ma of
  (Just r) -> Right r
  Nothing -> Left $ "Error " <> s

lineParse ::forall a. (String -> Either String a) -> String -> Either String (Array a)
lineParse parser str = traverseWithIndex 
  (\i s -> lmap (\msg -> "Line " <> (show (i+1)) <> ": " <> msg) (parser s)) 
  (readLines str)

toPairs :: forall a. Array a -> Maybe (Array (Tuple a a))
toPairs arr = case arr of
  [] -> Just []
  a -> do
    { head: x, tail: xs } <- uncons a
    { head: y, tail: ys } <- uncons xs
    rest <- toPairs ys
    pure $ Tuple x y : rest

unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust = unsafePartial fromJust

readInput :: Int -> Effect String
readInput num = readTextFile UTF8 $ "inputs/day" <> (show num) <> ".txt"

readLines :: String -> Array String
readLines s = filter (_ /= "") (lines s)

parseNum :: String -> Either String Int
parseNum s = case fromString s of 
  Just x -> Right x
  Nothing -> Left $ "Error: " <> s <> " cannot be parsed to int"