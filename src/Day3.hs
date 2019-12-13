{-# LANGUAGE LambdaCase, TupleSections #-}

module Day3 (part1, part2) where

import Data.List.Split
import qualified Data.Map as Map

import Lib
import Vec


data Dir
    = DUp
    | DDown
    | DLeft
    | DRight
    deriving Show

type Move = (Dir, Int)
type Point = Vec2 Int
type Length = Int
type Path = [(Point, Length)]


part1 :: IO Int
part1 = fmap (closestIntersectionBy manhattanDist) readInput
    where manhattanDist = sum . abs . fst

part2 :: IO Int
part2 = fmap (closestIntersectionBy combinedPathLength) readInput
    where combinedPathLength = uncurry (+) . snd

readInput :: IO String
readInput = readFile "inputs/day-3"

closestIntersectionBy
    :: Ord a => ((Point, (Length, Length)) -> a) -> String -> a
closestIntersectionBy f =
    minimum
        . map f
        . uncurry fastIntersect
        . both (coveredPoints . parseMoves)
        . head2
        . lines

parseMoves :: String -> [Move]
parseMoves = map parseMove . splitOn ","
  where
    parseMove s = (parseDir (head s), read (tail s))
    parseDir = \case
        'U' -> DUp
        'D' -> DDown
        'L' -> DLeft
        'R' -> DRight
        _ -> error "parseDir"

coveredPoints :: [Move] -> Path
coveredPoints = tail . scanl move ((Vec2 0 0), 0) . (interpolate =<<)
  where
    move (p, l) d = (move' p d, l + 1)
    move' p d = p + case d of
        DUp -> Vec2 0 1
        DDown -> Vec2 0 (-1)
        DLeft -> Vec2 (-1) 0
        DRight -> Vec2 1 0
    interpolate (d, n) = replicate n d

-- | O(n * log n), unlike Data.List.intersect, which is O(n^2)
fastIntersect :: Path -> Path -> [(Point, (Length, Length))]
fastIntersect p q =
    Map.toList (Map.intersectionWith (,) (Map.fromList p) (Map.fromList q))
