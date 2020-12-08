module Day8 where

import Prelude

import Data.Either (Either)
import Effect (Effect)
import Effect.Console (log)
import Utils (justToEither, readInput, readLines, unsafeFromJust)

data Ins = Nop | Acc Int | Jmp Int

-- parse :: String -> Either (Array Ins)
-- parse 

main :: Effect Unit
main = do
  input <- readInput 8
  log $ input
