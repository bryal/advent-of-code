module Day10 (part1, part2) where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Applicative
import Data.Maybe
import Data.List
import Data.Functor

import Lib
import Vec


type Point = Vec2 Int
type Asteroid = Point


part1 :: IO Int
part1 = fmap (snd . findBestLoc . parse) readInput

part2 :: IO Int
part2 = readInput <&> \s ->
    let
        as = parse s
        (p, _) = findBestLoc as
        -- They start counting from 1st, we start from 0th
        Vec2 x y = nthDestroyed 199 as p
    in (x * 100 + y)

findBestLoc :: Set Asteroid -> (Point, Int)
findBestLoc as =
    let countVisible = Set.size . detect as
    in maximumOn snd (map (\p -> (p, countVisible p)) (Set.toList as))

nthDestroyed :: Int -> Set Asteroid -> Point -> Asteroid
nthDestroyed n as p =
    let
        ds = detect as p
        n' = n - Set.size ds
        angleTo q =
            let v = q - p
            in (pi - foldr1 atan2 (fmap fromIntegral v)) :: Double
    in if n' <= 0
        then sortOn angleTo (Set.toList ds) !! n
        else nthDestroyed n' (Set.difference as ds) p

detect :: Set Asteroid -> Point -> Set Asteroid
detect as p =
    let
        closestBetween q =
            listToMaybe (filter (flip Set.member as) (pointsOnLine p q))
    in Set.fromList (catMaybes (map closestBetween (Set.toList as)))

pointsOnLine :: Point -> Point -> [Point]
pointsOnLine (Vec2 x0 y0) (Vec2 x1 y1) =
    let
        dx' = x1 - x0
        (dx, sx) = (abs dx', signum dx')
        dy = y1 - y0
        xs = filter (\x -> mod (x * dy) dx == 0) [1 .. dx]
        y x = div (x * dy) dx
    in if dx == 0
        then map (\y' -> (Vec2 x0 (y0 + signum dy * y'))) [1 .. abs dy]
        else map (\x -> (Vec2 (x0 + sx * x) (y0 + y x))) xs

parse :: String -> Set Asteroid
parse s = Set.fromList $ do
    (l, y) <- zip (lines s) [0 ..]
    (c, x) <- zip l [0 ..]
    if c == '#' then pure (Vec2 x y) else empty

readInput :: IO String
readInput = readFile "inputs/day-10"
