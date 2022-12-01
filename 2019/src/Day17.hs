module Day17 (part1) where

import qualified Data.Set as Set
import Data.Char
import Control.Applicative
import Vec
import Intcode

part1 :: IO Int
part1 = do
    s <- readInput
    let inp = parseIntcode s
        out = evalIntcode [] inp
    let img = map chr out
    let img' = lines img
    let scaffs = Set.fromList $ do
            (l, y) <- zip img' [0 ..]
            (c, x) <- zip l [0 ..]
            if c == '#' then pure (Vec2 x y) else empty
    let isects = flip filter (Set.toList scaffs) $ \scaff -> all
            (\v -> Set.member (scaff + v) scaffs)
            [Vec2 0 1, Vec2 0 (-1), Vec2 1 0, Vec2 (-1) 0]
    pure (sum (map product isects))

readInput :: IO String
readInput = readFile "inputs/day-17"
