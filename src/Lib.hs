module Lib (both, head2) where

import Data.Bifunctor

both :: Bifunctor p => (a -> b) -> p a a -> p b b
both f = bimap f f

head2 :: [a] -> (a, a)
head2 xs = (head xs, head (tail xs))
