{-# LANGUAGE LambdaCase #-}

module Day15 (part1, part2) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Functor
import Data.Maybe
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Either
import Vec
import Intcode

type Pos = Vec2 Int

part1 :: IO Int
part1 = fmap (fst . findO2System . parseIntcode) readInput

part2 :: IO Int
part2 = readInput <&> \s ->
    let
        pgm = parseIntcode s
        (_, o2Cont) = findO2System pgm
    in fromLeft (error "multiple O2 systems?") (bfs o2Cont)

findO2System :: Mem -> (Int, Continuation)
findO2System pgm =
    let (_, startCont) = stepIntcode pgm
    in fromRight (error "O2 system not found") (bfs (fromJust startCont))

-- | Breadth-first search to find either the time to fill the chamber with
--   oxygen (if starting in the position of the oxygen system), or the oxygen
--   system itself.
bfs :: Continuation -> Either Int (Int, Continuation)
bfs cont = bfs' Set.empty (Vec2 0 0, 0, cont) Seq.empty

bfs'
    :: Set Pos
    -> (Pos, Int, Continuation)
    -> Seq (Pos, Int, Continuation)
    -> Either Int (Int, Continuation)
bfs' visiteds (p, l, (Cont cont)) nexts =
    let
        dirs = filter (not . flip Set.member visiteds . snd) $ zip
            [1, 2, 3, 4]
            (map (p +) [Vec2 0 1, Vec2 0 (-1), Vec2 (-1) 0, Vec2 1 0])
        l' = l + 1
        bfs'' = \case
            [] -> Right []
            (dir, q) : ds -> case cont dir of
                ((_, [0]), _) -> bfs'' ds
                ((_, [1]), Just cont') -> fmap ((q, l', cont') :) (bfs'' ds)
                ((_, [2]), Just cont') -> Left cont'
                _ -> error "unexpected case in bfs''"
    in case bfs'' dirs of
        Left cont' -> Right (l', cont')
        Right ns -> case Seq.viewl (nexts Seq.>< Seq.fromList ns) of
            Seq.EmptyL -> Left l
            next Seq.:< nexts' -> bfs'
                (Set.union
                    (Set.fromList (p : (map (\(q, _, _) -> q) ns)))
                    visiteds
                )
                next
                nexts'

readInput :: IO String
readInput = readFile "inputs/day-15"
