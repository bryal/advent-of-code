module Lib (both, head2, minimumOn, maximumOn) where

import Data.Bifunctor
import Data.List

both :: Bifunctor p => (a -> b) -> p a a -> p b b
both f = bimap f f

head2 :: [a] -> (a, a)
head2 xs = (head xs, head (tail xs))

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn f = minimumBy (\a1 a2 -> compare (f a1) (f a2))

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f = maximumBy (\a1 a2 -> compare (f a1) (f a2))
