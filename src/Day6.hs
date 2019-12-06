{-# LANGUAGE TupleSections #-}

module Day6 (part1, part2) where

import Data.List.Split
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Bifunctor
import Data.Tuple
import Data.Sequence (Seq, (><))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set

import Lib

type Body = String
type OrbitedBy = Map Body [Body]

-- Part1

part1 :: IO Int
part1 = fmap (countOrbits "COM" 0 . parse1) readInput

readInput :: IO String
readInput = readFile "inputs/day-6"

parse1 :: String -> OrbitedBy
parse1 =
    Map.fromListWith (++) . map (second pure . head2 . splitOn ")") . lines

countOrbits :: Body -> Int -> OrbitedBy -> Int
countOrbits a l os = l + case Map.lookup a os of
    Just bs -> sum (map (\b -> countOrbits b (l + 1) os) bs)
    Nothing -> 0

-- Part 2

part2 :: IO Int
part2 = fmap (\inp -> (bfs "YOU" "SAN" (parse2 inp)) - 2) readInput

-- | Returns a directed graph with both forward- and backward-edges for each
--   orbit.
parse2 :: String -> Map Body [Body]
parse2 s =
    let
        forws = map (head2 . splitOn ")") (lines s)
        backws = map swap forws
    in Map.fromListWith (++) (map (second pure) (forws ++ backws))

bfs :: Body -> Body -> Map Body [Body] -> Int
bfs a = bfs' Set.empty (Seq.singleton (a, 0))

bfs' :: Set Body -> Seq (Body, Int) -> Body -> Map Body [Body] -> Int
bfs' visited as b os =
    let ((a, l), as') = viewl' as
    in
        if Set.member a visited
            then bfs' visited as' b os
            else if a == b
                then l
                else bfs'
                    (Set.insert a visited)
                    (as' >< Seq.fromList (map (, l + 1) (os Map.! a)))
                    b
                    os

viewl' :: Seq a -> (a, Seq a)
viewl' xs = case Seq.viewl xs of
    Seq.EmptyL -> error "viewl' of empty Seq"
    x Seq.:< xs' -> (x, xs')
