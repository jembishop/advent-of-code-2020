module Day8 where

import Prelude
import Control.Monad.Rec.Loops (iterateUntil)
import Control.Monad.State (State, evalState, get, put)
import Data.Array (index, length, span, uncons, (:), filter)
import Data.Either (Either(..))
import Data.HashSet (HashSet, empty, insert, member)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), split)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Utils (Error, justToEither, lineParse, numParse, readInput, unsafeFromJust)

data InsType
  = Nop
  | Acc
  | Jmp

derive instance insType :: Eq InsType

type Ins
  = Tuple InsType Int

instance showIns :: Show InsType where
  show = case _ of
    Nop -> "nop"
    Acc -> "acc"
    Jmp -> "jmp"

parseIns :: String -> Error Ins
parseIns s = do
  let
    sp = split (Pattern " ") s
  ins <- justToEither "ins" $ index sp 0
  num <- justToEither "num" $ index sp 1
  numP <- numParse num
  insTp <- case ins of
    "nop" -> Right Nop
    "acc" -> Right Acc
    "jmp" -> Right Jmp
    _ -> Left $ "Invalid instruction: " <> ins
  pure (Tuple insTp numP)

type HandHeld
  = { ins :: Array Ins, ip :: Int, acc :: Int, ipHist :: HashSet Int }

type Term
  = { looped :: Boolean, finished :: Boolean, acc :: Int }

type HandHeldS
  = State HandHeld Term

step :: HandHeldS
step = do
  st <- get
  let
    (Tuple ins n) = unsafeFromJust (index st.ins st.ip)
  let
    new =
      case ins of
        Nop -> st { ip = st.ip + 1 }
        Acc -> st { ip = st.ip + 1, acc = st.acc + n }
        Jmp -> st { ip = st.ip + n }
        # _ { ipHist = insert st.ip st.ipHist }
  let
    finished = new.ip >= length st.ins
  put new
  pure
    $ { looped: (member st.ip st.ipHist)
      , finished
      , acc: if finished then new.acc else st.acc
      }

findTerm :: Array Ins -> Term
findTerm ins =
  evalState (iterateUntil (\x -> x.finished || x.looped) step)
    { ins, ip: 0, acc: 0, ipHist: empty }

generateIns :: Array Ins -> Array (Array Ins)
generateIns arr =
  let
    inValid (Tuple ins n) = if ins == Acc then true else false

    swap (Tuple ins n) = Tuple newIns n
      where
      newIns = case ins of
        Nop -> Jmp
        Jmp -> Nop
        Acc -> Acc

    { init: i, rest: r } = span inValid arr
  in
    case uncons r of
      Just { head: x, tail: xs } ->
        (i <> (swap x : xs))
          : (map (\l -> i <> (x : l)) (generateIns xs))
      Nothing -> []

findPerm :: Array Ins -> Array Term
findPerm arr = filter _.finished $ map findTerm (generateIns arr)

main :: Effect Unit
main = do
  input <- readInput 8
  log $ show
    $ do
        parsed <- lineParse parseIns input
        let
          p1 = findTerm parsed
        let
          p2 = findPerm parsed
        pure $ Tuple p1 p2
