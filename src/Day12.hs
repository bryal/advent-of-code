module Day12 (part1, part2) where

import Control.Applicative
import Data.Foldable

import Parse
import Vec

type Pos = Vec3 Int
type Vel = Vec3 Int
type Moon = (Pos, Vel)

part1 :: IO Int
part1 = fmap (systemEnergy . stepN 1000 . parse) readInput
  where
    systemEnergy = sum . map moonEnergy
    moonEnergy m = potential m * kinetic m
    potential = sum . fmap abs . fst
    kinetic = sum . fmap abs . snd

part2 :: IO Int
part2 = fmap (periodByAxes . parse) readInput
  where
    periodByAxes ms =
        let
            (xms, yms, zms) = unzip3 $ map
                (\(Vec3 x y z, Vec3 a b c) -> ((x, a), (y, b), (z, c)))
                ms
            (xperiod, yperiod, zperiod) =
                (period xms, period yms, period zms)
        in leastCommonMultiple [xperiod, yperiod, zperiod]
    leastCommonMultiple = foldr1 lcm
    period ms = period' ms 1 (step ms)
    period' init n ms =
        if ms == init then n else period' init (n + 1) (step ms)

stepN :: Num a => Int -> [(a, a)] -> [(a, a)]
stepN n ms = iterate step ms !! n

step :: Num a => [(a, a)] -> [(a, a)]
step ms = map (\m -> stepMoon m ms) ms

stepMoon :: Num a => (a, a) -> [(a, a)] -> (a, a)
stepMoon (p, v) ms =
    let v' = foldl' (\u (q, _) -> u + signum (q - p)) v ms in (p + v', v')

parse :: String -> [Moon]
parse = map (\s -> (parsePos s, pure 0)) . lines
  where
    parsePos = applyParser $ liftA3
        Vec3
        (string "<x=" *> int)
        (string ", y=" *> int)
        (string ", z=" *> int <* string ">")

readInput :: IO String
readInput = readFile "inputs/day-12"
