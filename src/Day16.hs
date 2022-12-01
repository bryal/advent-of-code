{-# LANGUAGE LambdaCase #-}

module Day16 (part1, part2) where

import Data.Functor
import Data.Char

part1 :: IO String
part1 = fmap (take 8 . map intToDigit . nthPhase 100 . parse) readInput

nthPhase :: Int -> [Int] -> [Int]
nthPhase n inp = iterate phase inp !! n
  where
    phase inp =
        map (onesDigit . sum . zipWith (*) inp . pattern) [1 .. length inp]
    onesDigit x = abs (rem x 10)

pattern :: Int -> [Int]
pattern period = drop 1 $ cycle $ replicate period =<< [0, 1, 0, -1]

parse :: String -> [Int]
parse = map digitToInt

readInput :: IO String
readInput = readFile "inputs/day-16"

-- 1. The application of a pattern to a phase looks an awful lot like a matrix
--    multiplication, and indeed, we can model it as one.
--
-- 2. We don't have to take the ones-digit between repeated phases, as the least
--    significant digit will be preserved regardles of the other significant
--    digits of a multiplication. E.g.
--      onesDigit (2 * 7) = onesDigit (32 * 97) = onesDigit (-12) 7 = 4
--
-- 3. I know of at least a logarithmic time algorithm for rasing a square matrix
--    to a power by repeated squaring. We can make use of that.

part2 :: IO String
part2 = readInput <&> \s ->
    let
        inp = parse s
        offsetNs = take 7 inp
        offset = sum $ zipWith
            (\n i -> n * 10 ^ i)
            (reverse offsetNs)
            [(0 :: Int) ..]
        megaInp = drop offset (take (650 * 10000) (cycle inp))
    in map intToDigit (take 8 (nthPhase2 100 megaInp))

nthPhase2 :: Int -> [Int] -> [Int]
nthPhase2 n inp = iterate (fst . phase) inp !! n
  where
    phase = \case
        [] -> ([], 0)
        i : is' ->
            let
                (os, acc) = phase is'
                o = mod (acc + i) 10
            in (o : os, o)
