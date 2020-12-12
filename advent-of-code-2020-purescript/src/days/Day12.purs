module Day12 where

import Debug.Trace
import Prelude

import Data.Array (catMaybes, filter, findIndex, index, length, mapWithIndex, zip, (:), range, reverse)
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), isJust)
import Data.Ord (abs)
import Data.String (splitAt)
import Data.Traversable (any, sequence)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, clear)
import Partial.Unsafe (unsafeCrashWith)
import Utils (Error, errThrow, justToEither, lineParse, numParse, readFile, readInput)

type Pos = {x::Int, y:: Int} 

type Rot = Int

rotToPos :: Rot -> Pos
rotToPos i = case i `mod` 4 of
    0 -> {x: 0, y: 1} 
    1 -> {x: 1, y: 0} 
    2 -> {x: 0, y: -1} 
    3 -> {x: -1, y: 0} 
    _ -> unsafeCrashWith "impossible"

data Ins = CompMove Pos Int
    |Rotate Rot
    |Forward Int

derive instance genericDir :: Generic Ins _

instance showIns :: Show Ins where
  show = genericShow

type Boat
  = { pos :: Pos, dir:: Int}

addPos :: Pos -> Pos -> Pos
addPos p1 p2 = {x: p1.x + p2.x, y: p1.y + p2.y}

mulPos :: Pos -> Int -> Pos
mulPos p i = {x: i*p.x, y: i*p.y}

step :: Ins -> Boat -> Boat
step i b = case i of 
            CompMove p n -> {pos: (mulPos p n) + b.pos, dir: b.dir}
            Rotate n -> {pos: b.pos, dir: (b.dir + n)}
            Forward n -> {pos: b.pos + mulPos (rotToPos b.dir) n, dir: b.dir}
        -- where
        --   arrg = unsafePerformEffect (log $ show b)
rotateAbout :: Pos -> Rot -> Pos
rotateAbout pos rot =  newRel
    where
      newRel = case rot `mod` 4 of
            0 -> pos
            1 -> {x: pos.y, y: -pos.x}
            2 -> {x: -pos.x, y: -pos.y}
            3 -> {x: -pos.y, y: pos.x} 
            _ -> unsafeCrashWith "impossible"


        
type Boat2
  = { pos :: Pos, way:: Pos}

step2 :: Ins -> Boat2 -> Boat2
step2 i b = case i of 
            CompMove p n -> {pos: b.pos, way: (mulPos p n) + b.way}
            Rotate n -> {pos: b.pos, way: rotateAbout b.way n}
            Forward n -> {pos: b.pos + (mulPos (b.way) n), way: b.way}

degParse :: Int -> Error Int
degParse i = if i `mod` 90 /= 0 then Left "Must be mult of 90" else Right (i / 90)

parseIns :: String -> Error Ins
parseIns s = do
    num <- numParse sp.after 
    case sp.before of 
        "N" -> Right $ CompMove {x: 0, y: 1} num 
        "S" -> Right $ CompMove {x: 0, y: -1} num 
        "E"  -> Right $ CompMove {x: 1, y: 0} num 
        "W"  -> Right $ CompMove {x: -1, y: 0} num  
        "L"  -> map Rotate (degParse (-num)) 
        "R"  -> map Rotate (degParse num) 
        "F"  -> Right $ Forward num
        _ -> Left "Invalid dir" 
    where
       sp = splitAt 1 s

part1 :: Array Ins -> Boat
part1 ins = foldr step {pos: {x: 0, y: 0}, dir: 1} (reverse ins)

part2 :: Array Ins -> Boat2
part2 ins = foldr step2 {pos: {x: 0, y: 0}, way: {x: 10, y: 1}} (reverse ins)

parse :: String -> Error (Array Ins)
parse  = lineParse parseIns


main :: Effect Unit
main = do
  input <- readInput 12
  parsed <- errThrow $ parse input 
  let r1 = (part1 $ parsed)
  let r2 = (part2 $ parsed)
  log $ show ((abs r1.pos.x) + (abs r1.pos.y))
  log $ show ((abs r2.pos.x) + (abs r2.pos.y))