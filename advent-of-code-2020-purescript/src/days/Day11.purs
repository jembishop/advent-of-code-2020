module Day11 where

import Debug.Trace
import Prelude

import Control.Monad.Rec.Loops (iterateUntil)
import Control.Monad.ST (ST, for)
import Control.Monad.ST.Ref (read)
import Control.Monad.State (State, evalState, execState, get, put)
import Data.Array (catMaybes, filter, findIndex, index, length, mapWithIndex, zip, (:), range)
import Data.Array.ST (STArray, modify, peek, poke, run, thaw)
import Data.Either (Either(..))
import Data.Foldable (find, fold, foldr, surround)
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (traverse)
import Data.String (splitAt)
import Data.String.CodeUnits (fromCharArray, singleton, toCharArray)
import Data.Traversable (any, sequence)
import Data.Tuple (Tuple(..))
import Data.Tuple (uncurry)
import Effect (Effect)
import Effect.Console (log, clear)
import Effect.Unsafe (unsafePerformEffect)
import Utils (Error, errThrow, justToEither, readFile, readInput)

data Square = Floor
              | Empty
              | Occupied

derive instance eqSq :: Eq Square

squareToChar :: Square -> Char 
squareToChar = case _ of 
    Floor -> '.'
    Empty -> 'L'
    Occupied -> '#'

instance showSquare :: Show Square where
  show s = singleton $ squareToChar s


type Ferry = {squares :: Array Square, height:: Int, width:: Int}

type Pos = {x::Int, y:: Int} 

-- getSquare :: Ferry -> Pos -> Square
-- getSquare f p s = f.squares !! (f.height*p.x + p.y)
posToIdx :: Ferry -> Pos -> Int 
posToIdx f p = p.x + p.y*f.width

idxToPos :: Ferry -> Int -> Pos 
idxToPos f i = {x: i `mod` f.width, y: i / f.width}

addPos :: Pos -> Pos -> Pos
addPos p1 p2 = {x: p1.x + p2.x, y: p1.y + p2.y}

mulPos :: Pos -> Int -> Pos
mulPos p i = {x: i*p.x, y: i*p.y}

surrounds :: Array Pos
surrounds = [
  {x: 1, y: 1},
  {x: 1, y: 0},
  {x: 1, y: -1},
  {x: 0, y: -1},
  {x: -1, y: -1},
  {x: -1, y: 0},
  {x: -1, y: 1},
  {x: 0, y: 1}
]

adjacents :: Pos -> Array Pos 
adjacents pos = map (addPos pos) surrounds

directions :: Ferry -> Pos -> Array (Array Pos) 
directions f pos = map (\p -> map (\x -> (mulPos p x) + pos) (range 1 (maxLen pos.x p.x pos.y p.y))) surrounds
  where 
    maxLen x xdir y ydir = case ydir of 
      (-1) -> y 
      1 ->  f.height - y
      _ -> case xdir of 
        (-1) -> x
        1 -> f.width - x
        _ -> f.height


hitOcc :: Ferry -> Pos -> Maybe Boolean
hitOcc f p = 
  let pos = (posToIdx f p) in 
  if not $ p.x < f.width && p.x >= 0 && p.y < f.height && p.y >=0 then Just false
  else
  case index f.squares (posToIdx f p)  of 
  Just Occupied -> Just true
  Just Floor -> Nothing
  Just Empty -> Just false
  Nothing -> Just false

occVisible :: Ferry -> Array Pos -> Boolean
occVisible f arr = case join $ find isJust (map (hitOcc f) arr) of 
    (Just b) -> b
    Nothing -> false

numOccAdjacent :: Ferry -> Pos -> Int
numOccAdjacent f pos = length $ filter (_==Occupied) squares
   where 
    squares = catMaybes $ map (\x -> index f.squares (posToIdx f x))
     $ filter (\p -> p.x < f.width && p.x >= 0 && p.y < f.height && p.y >=0)
     (adjacents pos)

numOccVisible :: Ferry -> Pos -> Int
numOccVisible f pos = length $ filter (occVisible f) (directions f pos)

anyOccVisible :: Ferry -> Pos -> Boolean
anyOccVisible f pos = any (occVisible f) (directions f pos)

type UFunc = (Ferry -> Int -> Square -> Square) 
updateSquare1 :: UFunc
updateSquare1 f i s = case s of 
    Empty -> if nA ==0 then Occupied else Empty
    Occupied -> if nA >= 4 then Empty else Occupied
    Floor -> Floor
  where 
    nA = numOccAdjacent f (idxToPos f i)

updateSquare2 :: UFunc
updateSquare2 f i s = case s of 
    Empty -> 
      if nA ==0 then
         if not (anyOccVisible f pos)then
          Occupied else Empty
          else Empty
    Occupied -> if nA >= 5 then Empty else 
      let nV = numOccVisible f pos in 
        if nV >= 5 then Empty else Occupied
    Floor -> Floor
  where 
    pos = (idxToPos f i)
    nA = numOccAdjacent f pos

type FerryState = State Ferry Boolean

fib :: Int -> Int
fib n = if n == 1 || n == 2 then 1 else fib (n - 1) + fib (n - 2)

step :: UFunc ->  FerryState
step uf = do 
  oldFs <- get
  let newFs = update uf oldFs
  let argg = unsafePerformEffect (clear)
  let argg = unsafePerformEffect (log $ "------------------\n")
  let argg = unsafePerformEffect (log $ printFerry oldFs)
  let argg = unsafePerformEffect (log $ show $ numOccupied oldFs)
  let argg = unsafePerformEffect (log $ "------------------\n")
  let argg = unsafePerformEffect (log $ "------------------\n")
  -- let argg = unsafePerformEffect (log $ show $ fib 40)
  -- let argg = unsafePerformEffect (log $ show $ numOccupied oldFs)
  put newFs
  -- let diff = map (\(Tuple i x) -> Tuple (idxToPos newFs i) x) $ map (\(Tuple x y) -> x) $ filter (\(Tuple x y) -> x /= y) $ zip (mapWithIndex (\i -> \x -> Tuple i x) oldFs.squares ) (mapWithIndex (\i -> \x -> Tuple i x) newFs.squares) 
  -- let argg = unsafePerformEffect (log $ "DIFF" <> (show diff) <> "\n")
  -- let argg = unsafePerformEffect (log $ "------------------\n")
  pure $ oldFs == newFs

update :: UFunc -> Ferry -> Ferry
update ufunc ferry = ferry {squares = newSquares}
  where 
    newSquares = mapWithIndex 
      (ufunc ferry)
      ferry.squares 

charToSquare :: Char -> Error Square
charToSquare = case _ of
  '.' -> Right Floor
  'L' -> Right Empty
  '#' -> Right Occupied
  a -> Left $ "Invalid character: " <> (fromCharArray [a])


parse :: String -> Error Ferry 
parse s = do
  let sArr = toCharArray s
  width <- justToEither "no newline" $ findIndex (_=='\n') sArr
  squares <- sequence $ map charToSquare $ filter (_/='\n') sArr 
  pure $ {squares, width, height: (length squares) / width }

numOccupied :: Ferry -> Int
numOccupied f = length $ filter (_==Occupied) f.squares

steady ::UFunc -> Ferry -> Int
steady uf f = numOccupied stead
  where
    stead = execState (iterateUntil identity (step uf)) f

printFerry :: Ferry -> String 
printFerry f = fold mapped 
  where
    mapped = map (append "\n") $ splitEvery f.width $ fromCharArray $ map squareToChar f.squares
    
splitEvery :: Int -> String -> Array String
splitEvery n arr = case arr of  
  "" -> []
  arr -> before : (splitEvery n after)
  where
    {after,before} = splitAt n arr

main :: Effect Unit
main = do
  input <- readInput 11
  parsed <- errThrow $ parse input 
  let p1 = steady updateSquare1 parsed
  let p2 = steady updateSquare2 parsed
  pure unit

