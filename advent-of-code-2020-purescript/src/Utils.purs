module Utils where

import Prelude

import Data.Array (uncons, (:), filter, zipWith)
import Data.Array.NonEmpty (NonEmptyArray, index, toArray)
import Data.Array.NonEmpty as Na
import Data.Bifunctor (lmap)
import Data.BigInt as BigInt
import Data.Either (Either(..), fromRight)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), fromJust)
import Data.String.Utils (lines)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (throw)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Partial.Unsafe (unsafePartial)

type Error a = Either String a

justToEither :: forall a. String -> Maybe a -> Error a
justToEither s ma = case ma of
  (Just r) -> Right r
  Nothing -> Left $ "Error " <> s

lineParse ::forall a. (String -> Error a) -> String -> Error (Array a)
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

unsafeFromRight :: forall a. Error a -> a
unsafeFromRight = unsafePartial fromRight

readFile :: String -> Effect String
readFile s = readTextFile UTF8 $ s

readInput :: Int -> Effect String
readInput num = readFile $ "inputs/day" <> (show num) <> ".txt"

readLines :: String -> Array String
readLines s = filter (_ /= "") (lines s)

numParse :: String -> Error Int
numParse s = case fromString s of 
  Just x -> Right x
  Nothing -> Left $ "Error: <" <> s <> "> cannot be parsed to int"

bigIntParse :: String -> Error BigInt.BigInt
bigIntParse s = case BigInt.fromString s of 
  Just x -> Right x
  Nothing -> Left $ "Error: " <> s <> " cannot be parsed to bigint"

uIdxN :: forall a . NonEmptyArray a -> Int -> a
uIdxN a i = unsafeFromJust (index a i)

errThrow :: forall a . forall b . Show a => Either a b -> Effect b
errThrow = case _ of
  Left s -> throw (show s)
  Right x -> pure x

diff :: forall a . Ring a => NonEmptyArray a -> Array a 
diff arr =  zipWith (-) shifted (toArray arr) 
    where {head:_, tail: shifted} = Na.uncons arr 