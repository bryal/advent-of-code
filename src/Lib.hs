{-# LANGUAGE LambdaCase #-}

module Lib
    ( both
    , head2
    , head3
    , minimumOn
    , maximumOn
    , viewl'
    , everyNth
    , bsearchIntMax
    )
where

import Data.Bifunctor
import Data.List
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

both :: Bifunctor p => (a -> b) -> p a a -> p b b
both f = bimap f f

head2 :: [a] -> (a, a)
head2 xs = (head xs, head (tail xs))

head3 :: [a] -> (a, a, a)
head3 xs = (head xs, head (tail xs), head (tail (tail xs)))

minimumOn :: Ord b => (a -> b) -> [a] -> a
minimumOn f = minimumBy (\a1 a2 -> compare (f a1) (f a2))

maximumOn :: Ord b => (a -> b) -> [a] -> a
maximumOn f = maximumBy (\a1 a2 -> compare (f a1) (f a2))

viewl' :: Seq a -> (a, Seq a)
viewl' xs = case Seq.viewl xs of
    Seq.EmptyL -> error "viewl' of empty Seq"
    x Seq.:< xs' -> (x, xs')

everyNth :: Int -> [a] -> [a]
everyNth n = \case
    a : as -> a : everyNth n (drop (n - 1) as)
    _ -> []

bsearchIntMax :: (Int -> Bool) -> Int -> Int -> Int
bsearchIntMax p low high = if low < high - 1
    then
        let mid = low + div (high - low) 2
        in uncurry (bsearchIntMax p) $ if p mid then (mid, high) else (low, mid)
    else if p high then high else low
