module Day14 where

import Data.Long
import Data.Long.Bits
import Debug.Trace
import Prelude

import Control.Alternative ((<|>))
import Control.Apply (lift2)
import Data.Array (replicate, uncons, updateAt, zipWith, (:), length)
import Data.Array.NonEmpty as NE
import Data.Either (Either(..))
import Data.Foldable (foldr, maximum, minimum, minimumBy, foldl)
import Data.Functor (map)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (binary)
import Data.Long (fromInt, Long)
import Data.Long.Internal (fromStringAs, toStringAs)
import Data.Map (Map, empty, insert)
import Data.Maybe (Maybe(..), isJust)
import Data.Ord (abs)
import Data.String (Pattern(..), split, splitAt)
import Data.String.CodeUnits (fromCharArray, singleton, toCharArray)
import Data.String.Regex (Regex, match, regex)
import Data.String.Regex.Flags (RegexFlags(..))
import Data.Traversable (any, sequence, sum)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log, clear)
import Effect.Unsafe (unsafePerformEffect)
import Partial.Unsafe (unsafeCrashWith)
import Utils (Error, errThrow, justToEither, lineParse, numParse, readFile, readInput, unsafeFromJust, unsafeFromRight)

type BitMask = {z:: Long, o:: Long} 

data Ins = Mask String | Set {idx:: Long,  val::Long}

instance showIns :: Show Ins where
    show = case _ of 
        Mask i -> "Mask " <> (show i)
        Set {idx, val} -> "Set " <> (show idx) <> " " <> (show val)

memRegex :: Regex
memRegex = unsafeFromRight $ regex """^mem\[(\d+)\] = (\d+)""" mempty

maskRegex :: Regex
maskRegex = unsafeFromRight $ regex """^mask = (.+)""" mempty

parseMem :: String -> Error Ins
parseMem s = do
  res <- justToEither "no matches" $ match memRegex s
  idx <- ((justToEither "" $ NE.index res 1) >>= justToEither "") >>= numParse
  val <- ((justToEither "" $ NE.index res 2) >>= justToEither "") >>= numParse
  pure (Set {idx: fromInt idx, val: fromInt val})

convertMask :: String -> Char -> Error Long
convertMask m c = justToEither ( singleton c <> " mask " <> show m) $ 
                fromStringAs binary  $ fromCharArray 
                $ map (\x -> if x == 'X' then c else x) (toCharArray m)

maskToBitMask :: String -> Error BitMask
maskToBitMask maskR = do
  z <- convertMask maskR '0' 
  o <- convertMask maskR '1' 
  pure ({z, o})

parseMask :: String -> Error Ins
parseMask s = do
  res <- justToEither "no matches" $ match maskRegex s
  maskR <- ((justToEither "" $ NE.index res 1) >>= justToEither "") 
  pure (Mask maskR)
    where
        isX c = if c == "X" then Right Nothing else Left "not X"

applyMask :: BitMask -> Long -> Long
applyMask m a = (m.z .&. m.o) .|.(m.z .&. a) .|.(m.o .&. a)


parseIns :: String -> Error Ins
parseIns s = (parseMem s) <|> (parseMask s)

parse :: String -> Error (Array Ins)
parse = lineParse parseIns 

type Memory = {mem:: Map Long Long, mask:: String}

writeMem:: Memory -> Ins -> Memory
writeMem m ins = case ins of 
    Mask ma -> m {mask = ma}
    Set {idx, val} -> m {mem = insert idx (applyMask (unsafeFromRight $ maskToBitMask m.mask) val) m.mem}

genIdxs :: Array Char -> Array (Array Char)
genIdxs a = case uncons a of 
    Just {head, tail} -> case head of 
                'X' -> (map ('1' : _) (genIdxs tail)) <> (map ('0' : _) (genIdxs tail))
                o  -> map (o : _) (genIdxs tail)
    Nothing -> [[]] 

applyMask2 :: Array Char -> Array Char -> Array Char
applyMask2 addr mask = zipWith go padded mask
    where 
        go i j = case j of 
            '0' -> i
            'X' -> 'X'
            _ -> '1'
        padded = replicate ((length mask) - (length addr) ) '0' <> addr
        -- arrrg = unsafePerformEffect (log $ "PADDED" <> (show padded))
        -- arrrg2 = unsafePerformEffect (log $ "ZIPPED" <> (show $ zipWith go padded mask))
        -- arrrg3 = unsafePerformEffect (log $ "MASK  " <> (show $ mask))

type Runner = Memory -> Ins -> Memory

writeMem2:: Runner
writeMem2 m ins = case ins  of 
    Mask ma -> m {mask = ma}
    Set {idx, val} -> m {mem = foldr go m.mem $ genIdxs (applyMask2 (toCharArray $ toStringAs binary idx) (toCharArray m.mask))}
        where
            go comb mapp = insert ((unsafeFromJust<<<fromStringAs binary<<<fromCharArray) comb) val mapp
            -- lr = unsafePerformEffect (log $ show m.mem)
          

run :: Runner -> Array Ins -> Memory
run runn arr = foldl runn {mem: empty, mask: "00000"} arr

part1 :: Array Ins -> Long
part1 arr = sum $ _.mem $ run writeMem arr

part2 :: Array Ins -> Long
part2 arr = sum $ _.mem $ run writeMem2 arr

main :: Effect Unit
main = do
  input <- readInput 14
--   input <- readFile "inputs/test.txt"
  parsed <- errThrow $ parse input 
  log $ show $ part1 parsed
  log $ show $ part2 parsed