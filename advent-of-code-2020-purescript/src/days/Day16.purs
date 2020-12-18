module Day16 where

import Data.HashSet
import Prelude

import Data.Array (all, concat, filter, head, index, init, length, sort, tail, (:), replicate)
import Data.BigInt (fromInt, BigInt)
import Data.Foldable (foldl)
import Data.Functor (map)
import Data.FunctorWithIndex (mapWithIndex)
import Data.HashMap (HashMap, delete, fromFoldable, insert, lookup, size, toArrayBy, update)
import Data.HashSet as HS
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..), split, splitAt)
import Data.String.Utils (startsWith)
import Data.Traversable (any, sequence, sum)
import Data.Tuple (Tuple(..), snd, fst)
import Effect (Effect)
import Effect.Console (log, clear)
import Utils (Error, errThrow, justToEither, lineParse, numParse, readFile, readInput, unsafeFromJust, unsafeFromRight)

type Ticket = Array Int

type Range = {from:: Int, to:: Int}

type Policy = {field:: String, range1:: Range, range2:: Range}

type Input = {policies:: Array Policy, myTicket:: Ticket, nearbyTickets:: Array Ticket}

parseRange :: String -> Error Range
parseRange s = do
    let sp = split (Pattern "-") s  
    from <- (justToEither "rng from" $ index sp 0) >>= numParse
    to <- (justToEither "rng to" $ index sp 1) >>= numParse
    pure {from: from, to: to}


parsePolicy :: String -> Error Policy
parsePolicy s = do 
    let sp = split (Pattern ": ") s  
    field <- justToEither "no field" $ index sp 0
    rngs <- justToEither "no ranges" $ index sp 1
    let rngSp = split (Pattern " or ") rngs 
    r1 <- justToEither "no rng1" $ index rngSp 0
    r2 <- justToEither "no rng2" $ index rngSp 1
    range1 <- parseRange r1 
    range2 <- parseRange r2
    pure {field, range1, range2}

parseTicket :: String -> Error Ticket
parseTicket s = sequence $ map numParse (split (Pattern ",") s)



parse :: String -> Error Input
parse s = do 
    let paras = split (Pattern "\n\n") s
    p <- justToEither "no policies" $ index paras 0
    policies <- lineParse parsePolicy p 
    myT <- justToEither "ticket" $ do 
            x <- index paras 1
            index (split (Pattern "\n") x) 1

    myTicket <- parseTicket myT 
    nT <- justToEither "nearbys" $ do
            x <- index paras 2
            t <- tail (split (Pattern "\n") x) 
            pure $ filter (_/="") t
    nearbyTickets <- sequence $ map parseTicket nT
    pure {policies, myTicket, nearbyTickets}

inRange :: Range -> Int -> Boolean
inRange r i = (i <= r.to) && (i >= r.from)

fieldViolations :: Array Policy -> Int -> Array String
fieldViolations p i = map _.field $ filter (\x -> not $ (inRange x.range1 i) || (inRange x.range2 i)) p

validTicket :: Array Policy -> Ticket -> Boolean
validTicket pols t = not $ any ((_==length pols)<<<length) $ map (fieldViolations pols) t

-- map field name, possible indicies
type Hypothesis = HashMap Int (HashSet String) 

updateHypothesis :: Array Policy -> Hypothesis -> Ticket -> Hypothesis
updateHypothesis pols hypo t = foldl adjust hypo violations
    where 
        adjust hyp violation = update (\v -> (Just $ HS.delete violation.field v)) violation.idx hyp
        violations = concat $ mapWithIndex (\i -> \x -> map {idx :i, field: _} (fieldViolations pols x)) t

deducePossibleFields :: Array Policy -> Array Ticket -> Hypothesis
deducePossibleFields pols tics = foldl (updateHypothesis pols) initial tics
    where 
       initial = fromFoldable $ mapWithIndex Tuple $ replicate (length fields) (HS.fromFoldable fields)
       fields = (map _.field pols)

deduceFields :: Hypothesis -> Maybe (Array (Tuple Int String)) 
deduceFields h = if size h == 0 then Just [] else do
        let arr = toArrayBy Tuple h
        Tuple i s <- head $ filter ((_==1)<<<HS.size<<<snd) arr
        toRemove <- head $ HS.toArray s
        let new = map (HS.delete toRemove) (delete i h)
        rest <- deduceFields new
        pure ((Tuple i toRemove) : rest)

part2 :: Input -> String -> BigInt
part2 i prefix = foldl (*) one onlyDeparture
    where 
        fieldOrder = fromFoldable $ unsafeFromJust $ deduceFields $ deducePossibleFields i.policies validNearby
        myFields = mapWithIndex (\i -> \x -> Tuple (unsafeFromJust $ lookup i fieldOrder) x)  i.myTicket
        validNearby = filter (validTicket i.policies) i.nearbyTickets 
        onlyDeparture = map (fromInt<<<snd) $ filter (\t -> startsWith prefix (fst t)) myFields


ticketErrRate ::  Array Policy -> Ticket -> Int
ticketErrRate pol tic = sum $ map (\x -> if (fieldViolations pol x) == [] then 0 else x) tic

numValid :: Array Policy -> Array Ticket -> Int
numValid pol = sum <<< map (ticketErrRate pol)

part1 :: Input -> Int
part1 i = numValid i.policies i.nearbyTickets


main :: Effect Unit
main = do
  input <- readInput 16
  parsed <- errThrow $ parse input 
  log $ show $ part2 parsed "departure"