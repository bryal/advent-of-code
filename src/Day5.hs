{-# LANGUAGE LambdaCase, TemplateHaskell, TypeApplications #-}

module Day5 (part1, part2) where

import Data.List.Split
import qualified Data.Sequence as Seq

import Intcode


part1 :: IO [Int]
part1 = fmap (evalIntcode [1] . parse) readInput

part2 :: IO [Int]
part2 = fmap (evalIntcode [5] . parse) readInput

readInput :: IO String
readInput = readFile "inputs/day-5"

parse :: String -> Mem
parse = Seq.fromList . fmap read . splitOn ","
