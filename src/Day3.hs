{-# LANGUAGE LambdaCase, TupleSections #-}

module Day3 (part1, part2) where

import Data.List.Split
import qualified Data.Map as Map

import Lib

data Dir
    = DUp
    | DDown
    | DLeft
    | DRight
    deriving Show

type Move = (Dir, Int)
data Point = Point Int Int deriving (Eq, Ord)
type Length = Int
type Path = [(Point, Length)]

-- Part 1

part1 :: IO Int
part1 = fmap (closestIntersectionBy manhattanDist) readInput

readInput :: IO String
readInput = readFile "inputs/day-3"

manhattanDist :: (Point, (Length, Length)) -> Int
manhattanDist ((Point x y), _) = abs x + abs y

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
coveredPoints = tail . scanl move ((Point 0 0), 0) . (interpolate =<<)
  where
    move (p, l) d = (move' p d, l + 1)
    move' p d = addPoint p $ case d of
        DUp -> (0, 1)
        DDown -> (0, -1)
        DLeft -> (-1, 0)
        DRight -> (1, 0)
    interpolate (d, n) = replicate n d

addPoint :: Point -> (Int, Int) -> Point
addPoint (Point x y) (dx, dy) = Point (x + dx) (y + dy)

-- | O(n * log n), unlike Data.List.intersect, which is O(n^2)
fastIntersect :: Path -> Path -> [(Point, (Length, Length))]
fastIntersect p q =
    Map.toList (Map.intersectionWith (,) (Map.fromList p) (Map.fromList q))

-- Part 2

part2 :: IO Int
part2 = fmap (closestIntersectionBy combinedPathLength) readInput

combinedPathLength :: (Point, (Length, Length)) -> Length
combinedPathLength = uncurry (+) . snd
