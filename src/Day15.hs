{-# LANGUAGE LambdaCase #-}

module Day15 (part1, part2) where

import qualified Data.Set as Set
import Data.Functor
import Data.Maybe
import Data.List
import qualified Data.Sequence as Seq
import Vec
import Intcode

type PathLength = Int
type Tile = Int

part1 :: IO Int
part1 = fmap (fst . findO2System . parseIntcode) readInput

part2 :: IO Int
part2 = readInput <&> \s ->
    let
        pgm = parseIntcode s
        (_, o2Cont) = findO2System pgm
        (_, l, _) = last (bfs o2Cont)
    in l

findO2System :: Mem -> (PathLength, Continuation)
findO2System pgm =
    let
        (_, startCont) = stepIntcode pgm
        (_, l, c) =
            fromJust (find (\(t, _, _) -> t == 2) (bfs (fromJust startCont)))
    in (l, c)

bfs :: Continuation -> [(Tile, PathLength, Continuation)]
bfs c = bfs' Set.empty (0, Vec2 0 0 :: Vec2 Int, 0, c) Seq.empty
  where
    bfs' visiteds (tile, p, l, Cont cont) nexts =
        let
            dirs = filter (not . flip Set.member visiteds . snd) $ zip
                [1, 2, 3, 4]
                (map (p +) [Vec2 0 1, Vec2 0 (-1), Vec2 (-1) 0, Vec2 1 0])
            l' = l + 1
            neighbours = filter (\(t, _, _, _) -> t /= 0) $ map
                (\(dir, p') ->
                    let ((_, tile'), cont') = cont dir
                    in (head tile', p', l', fromJust cont')
                )
                dirs
            visiteds' = Set.union
                (Set.fromList (p : map (\(_, p', _, _) -> p') neighbours))
                visiteds
        in
            (tile, l, Cont cont)
                : case Seq.viewl (nexts Seq.>< Seq.fromList neighbours) of
                    Seq.EmptyL -> []
                    next Seq.:< nexts' -> bfs' visiteds' next nexts'

readInput :: IO String
readInput = readFile "inputs/day-15"
