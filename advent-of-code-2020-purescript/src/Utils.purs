module Utils where

import Prelude
import Data.Array (uncons, cons, (:))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromJust)
import Data.Tuple (Tuple(..))
import Partial.Unsafe (unsafePartial)

tryArr :: forall a. (String -> Either String a) -> Array String -> Either String (Array a)
tryArr p s = go 0 p s
  where
  go n p s = case uncons s of
    Just { head: h, tail: t } -> case p h of
      Right x -> map (cons x) (go (n + 1) p t)
      Left s -> Left ("Error at #" <> (show n) <> ": " <> s)
    Nothing -> Right []

justToEither :: forall a. String -> Maybe a -> Either String a
justToEither s ma = case ma of
  (Just r) -> Right r
  Nothing -> Left ("Error " <> s)

toPairs :: forall a. Array a -> Maybe (Array (Tuple a a))
toPairs arr = case arr of
  [] -> Just []
  a -> do
    { head: x, tail: xs } <- uncons a
    { head: y, tail: ys } <- uncons xs
    rest <- toPairs ys
    pure ((Tuple x y) : rest)

unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust = unsafePartial fromJust
