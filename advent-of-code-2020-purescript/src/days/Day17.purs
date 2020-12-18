module Day17 where

import Data.FunctorWithIndex
import Debug.Trace
import Prelude

import Control.Monad.Rec.Loops (iterateUntil)
import Control.Monad.State (State, evalState, execState, get, put)
import Control.MonadZero (guard)
import Data.Array (catMaybes, concat, filter, findIndex, index, length, mapWithIndex, range, replicate, updateAt, zip, (..), (:))
import Data.Either (Either(..))
import Data.Foldable (find, fold, foldr, sum, surround)
import Data.HashSet (fromArray, toArray)
import Data.HashSet as HS
import Data.Maybe (Maybe(..), isJust)
import Data.Newtype (class Newtype, traverse, unwrap, wrap)
import Data.String (Pattern(..), split, splitAt)
import Data.String.CodeUnits (fromCharArray, singleton, toCharArray)
import Data.Traversable (any, foldl, for, sequence)
import Data.Tuple (Tuple(..))
import Data.Tuple (uncurry)
import Effect (Effect)
import Effect.Console (log, clear)
import Effect.Unsafe (unsafePerformEffect)
import Node.Stream (onFinish)
import Utils (Error, errThrow, justToEither, readFile, readInput)

type Point3 = {x::Int, y::Int, z :: Int} 

type Point4 = {x::Int, y::Int, z:: Int, w:: Int} 

type Cubes3 = HS.HashSet Point3

type Cubes4 = HS.HashSet Point4

neighbours3 :: Point3 -> Array Point3
neighbours3 p = do 
  x <- -1..1
  y <- -1..1
  z <- -1..1
  let point = {x, y ,z} 
  guard $ point /= {x:0,y:0,z:0}
  pure $ point + p

neighbours4 :: Point4 -> Array Point4
neighbours4 p = do 
  x <- -1..1
  y <- -1..1
  z <- -1..1
  w <- -1..1
  let point = {x, y ,z, w} 
  guard $ point /= {x:0,y:0,z:0, w:0}
  pure $ point + p

numNeighbors3 :: Cubes3 -> Point3 -> Int
numNeighbors3 cs p = length $ filter (\x -> HS.member x cs) (neighbours3 p)

numNeighbors4 :: Cubes4 -> Point4 -> Int
numNeighbors4 cs p = length $ filter (\x -> HS.member x cs) (neighbours4 p)


charToCube :: Char -> Boolean
charToCube = case _ of
  '#' -> true
  _ -> false

rule3 :: Cubes3 -> Point3 -> Boolean 
rule3 cs p = case HS.member p cs of 
  true -> case n of 
    2 -> true 
    3 -> true 
    _ -> false
  false -> case n of 
    3 -> true 
    _ -> false
  where 
    n = numNeighbors3 cs p

rule4 :: Cubes4 -> Point4 -> Boolean 
rule4 cs p = case HS.member p cs of 
  true -> case n of 
    2 -> true 
    3 -> true 
    _ -> false
  false -> case n of 
    3 -> true 
    _ -> false
  where 
    n = numNeighbors4 cs p


step3 :: Cubes3 -> Int -> Cubes3
step3 cs _ = fromArray $ filter (rule3 cs) pointsToCheck 
  where 
    toA = toArray cs
    pointsToCheck = toA <> do 
      point <- toA
      neighbours3 point

step4 :: Cubes4 -> Int -> Cubes4
step4 cs _ = fromArray $ filter (rule4 cs) pointsToCheck 
  where 
    toA = toArray cs
    pointsToCheck = toA <> do 
      point <- toA
      neighbours4 point
      
run3 :: Int -> Cubes3 -> Cubes3
run3 i cs = foldl step3 cs (0..i)

run4 :: Int -> Cubes4 -> Cubes4
run4 i cs = foldl step4 cs (0..i)

parse :: String -> Cubes3
parse s = fromArray $ map (\e -> {x: e.x, y: e.y, z: 0}) filt
        where chars = map toCharArray (split (Pattern "\n") s)
              idxed = concat $ mapWithIndex 
                  (\x -> \xv -> 
                  (mapWithIndex (\y -> \val -> {x: x, y: y, val: val}) xv) 
                  ) chars
              filt = filter (\e -> charToCube e.val) idxed

to4 :: Cubes3 -> Cubes4
to4 = HS.map (\e -> {x: e.x, y: e.y, z: e.z, w: 0})

main :: Effect Unit
main = do
  input <- readInput 17
  -- input <- readFile "inputs/test.txt"
  let parsed = parse input 
  log $ show $ HS.size (run3 5 parsed) 
  log $ show $ HS.size (run4 5 (to4 parsed)) 
