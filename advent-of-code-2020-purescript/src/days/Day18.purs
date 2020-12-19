module Day18 where

import Data.Either
import Prelude hiding (between,when)
import Text.Parsing.Parser
import Text.Parsing.Parser.String

import Control.Alternative (class Alt, class Alternative, class Plus, (<|>))
import Control.Lazy (class Lazy, fix)
import Control.Monad.Rec.Loops (iterateUntil)
import Control.Monad.State (State, evalState, get, put)
import Data.Array (many, filter)
import Data.Bifunctor (lmap)
import Data.BigInt (BigInt, fromInt, toNumber)
import Data.Char.Unicode (isDigit)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Int (fromString)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Semigroup.Foldable (maximum, minimum)
import Data.String.CodeUnits (fromCharArray, singleton, toCharArray, uncons)
import Data.Traversable (any, foldr, sum)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Text.Parsing.Parser.Combinators (between, chainl, chainr1, chainl1, endBy1, many1Till, optionMaybe, sepBy1, try)
import Text.Parsing.Parser.Token (digit)
import Text.Parsing.String.Repetition (greedy)
import Utils (Error, bigIntParse, errThrow, justToEither, lineParse, numParse, readInput, uIdxN, unsafeFromJust)


data OpType = Add | Mult
data Expr = Lit BigInt | Op OpType Expr Expr 

instance se :: Show Expr where 
    show = case _ of 
        Lit i -> show (toNumber i)
        Op ot e1 e2 -> "(" <> (show e1) <> opS <> (show e2) <> ")"
            where
             opS = case ot of 
                Add -> "+"
                Mult -> "*"

type SParser a = Parser String a 

parenP :: forall a . SParser a -> SParser a 
parenP = between (string "(") (string ")")

litP :: SParser Expr
litP =  do 
    ds <- many digit
    let st = fromCharArray ds 
    i <- case fromString st of 
        Just i -> pure i
        Nothing -> fail ("could nor parse " <> show st  <> " to number")
    pure $ Lit (fromInt i)


addP :: SParser (Expr -> Expr -> Expr) 
addP = fix \f -> Op Add <$ string "+" 

mulP :: SParser (Expr -> Expr -> Expr) 
mulP = fix \f -> Op Mult <$ string "*" 


exprParse :: SParser Expr
exprParse = fix $ \p -> (chainl1 (litP <|> parenP exprParse) (mulP <|> addP)) 

exprParse2 :: SParser Expr
exprParse2 = fix $ \p -> expr0
    where 
        expr0 = fix \p -> chainl1 expr1 mulP
        expr1 = fix \p -> chainl1 term addP
        term = fix \p -> (litP <|> parenP expr0) 


parseE :: SParser Expr -> String -> Either ParseError Expr 
parseE p s = runParser (fromCharArray (filter (_/=' ') (toCharArray s))) p


eval :: Expr -> BigInt
eval = case _ of
    Op ot e1 e2 -> case ot of 
       Add -> (eval e1) + (eval e2)
       Mult -> (eval e1) * (eval e2)
    Lit e -> e

parse :: SParser Expr -> String -> Error (Array Expr)
parse p = lineParse (lmap show <<<parseE p)

main :: Effect Unit
main = do
  input <- readInput 18
  parsed <- errThrow $ parse exprParse input
  log $ show $ sum $ map eval parsed
  parsed <- errThrow $ parse exprParse2 input
  log $ show $ sum $ map eval parsed