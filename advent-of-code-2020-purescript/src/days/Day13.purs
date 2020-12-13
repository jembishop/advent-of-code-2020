module Day13 where

import Debug.Trace
import Prelude

import Control.Alternative ((<|>))
import Data.Array (catMaybes, filter, findIndex, index, length, mapWithIndex, zip, (:), range, reverse)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (traverse)
import Data.Ord (abs)
import Data.String (Pattern(..), split, splitAt)
import Data.Traversable (any, sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, clear)
import Partial.Unsafe (unsafeCrashWith)
import Utils (Error, errThrow, justToEither, lineParse, numParse, readFile, readInput)


parseBus :: String -> Error (Maybe Int)
parseBus s = isX s <|> (Just <$> (numParse s))
      where
        isX c = if c == "x" then Right Nothing else Left "not x"

parse :: String -> Error {timestamp:: Int, buses::Array (Maybe Int)}
parse s = do
    let sp = split (Pattern "\n") s
    fh <- justToEither "fl" $ index sp 0
    timestamp <- numParse fh
    sh <- justToEither "sl" $ index sp 1
    buses <- sequence $ map parseBus $ split (Pattern ",") sh
    pure {timestamp, buses}



main :: Effect Unit
main = do
  input <- readInput 13
  parsed <- errThrow $ parse input 
  log $ show parsed