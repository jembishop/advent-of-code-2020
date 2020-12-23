module Day19 where

import Prelude
import Control.Alternative ((<|>))
import Data.Array (catMaybes, concat, concatMap, delete, drop, elem, filter, find, foldl, index, length, mapWithIndex, nub, range, replicate, reverse, some, sortBy, take, uncons, (..), (:))
import Data.Char.Unicode (isLetter)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HashMap (HashMap, fromFoldable, insert, lookup)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Console (log)
import Text.Parsing.Parser (Parser, fail, runParser)
import Text.Parsing.Parser.Combinators (try)
import Text.Parsing.Parser.String (eof, satisfy, string, whiteSpace)
import Text.Parsing.Parser.Token (digit)
import Utils (Error, errThrow, justToEither, lineParse, readFile, readInput, unsafeFromJust)

data RuleTree
  = Leaf Char
  | Node Int (Array (Array RuleTree))

data Rule
  = Letter Char
  | Compound (Array (Array Int))

derive instance genRule :: Generic Rule _

instance name :: Show Rule where
  show = genericShow

instance hello :: Show RuleTree where
  show = case _ of
    Leaf c -> show c
    Node i arr -> "(" <> show i <> " " <> show arr <> ")"

type SParser a
  = Parser String a

type Input
  = { rules :: HashMap Int Rule, messages :: Array String }

toRuleTree :: HashMap Int Rule -> Int -> RuleTree
toRuleTree hm i = case unsafeFromJust $ lookup i hm of
  Letter c -> Leaf c
  Compound arr -> Node i (map (map (toRuleTree hm)) arr)

validate :: Array Char -> RuleTree -> Array (Array Char)
validate s rt = case uncons s of
  Just { head, tail } -> do
    case rt of
      Leaf c -> if head == c then [ tail ] else []
      Node idx arr -> join $ map (go s) arr
  Nothing -> []
  where
  go s match = case uncons match of
    Nothing -> [ s ]
    Just { head: h1, tail: t1 } -> do
      rest <- validate s h1
      go rest t1

intP :: SParser Int
intP = do
  ds <- some digit
  let
    st = fromCharArray ds
  i <- case fromString st of
    Just i -> pure i
    Nothing -> fail ("could nor parse " <> show st <> " to number")
  pure $ i

letP :: SParser Rule
letP = do
  _ <- whiteSpace
  _ <- string "\""
  c <- satisfy isLetter
  _ <- string "\""
  _ <- whiteSpace
  eof
  pure $ Letter c

arrP :: SParser (Array Int)
arrP =
  some do
    _ <- whiteSpace
    i <- intP
    _ <- whiteSpace
    pure i

oneP :: SParser Rule
oneP = do
  a <- arrP
  eof
  pure $ Compound [ a ]

twoP :: SParser Rule
twoP = do
  n1 <- arrP
  _ <- string "|"
  _ <- whiteSpace
  n2 <- arrP
  pure $ Compound [ n1, n2 ]

ruleP :: SParser Rule
ruleP = (try letP) <|> (try oneP) <|> (try twoP)

ruleEntryP :: SParser (Tuple Int Rule)
ruleEntryP = do
  idx <- intP
  _ <- string ":"
  rule <- ruleP
  pure $ Tuple idx rule

parse :: String -> Error Input
parse s = do
  let
    sp = split (Pattern "\n\n") s
  r <- justToEither "fh" $ index sp 0
  rules <- lineParse (\s -> runParser s ruleEntryP) r
  m <- justToEither "sh" $ index sp 1
  let
    messages = filter (_ /= "") $ split (Pattern "\n") m
  pure $ { rules: fromFoldable rules, messages }

part1 :: Input -> Int
part1 inp = length $ filter (elem []) $ map (\x -> validate (toCharArray x) (ruleTree 0)) inp.messages
  where
  ruleTree = toRuleTree inp.rules

makeRepeated :: Int -> Array Int -> Array (Array Int)
makeRepeated n arr = map (\n -> join $ map (replicate n) arr) (range 1 n)

updateInput :: Int -> Input -> Input
updateInput n inp =
  inp
    { rules =
      insert 11
        (Compound (reverse $ makeRepeated n [ 42, 31 ]))
        (insert 8 (Compound (reverse $ makeRepeated n [ 42 ])) inp.rules)
    }

main :: Effect Unit
main = do
  input <- readInput 19
  input <- errThrow $ parse input
  let
    upInp = updateInput 10 input
  log $ show $ part1 input
  log $ show $ part1 upInp
