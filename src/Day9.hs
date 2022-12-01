{-# LANGUAGE LambdaCase, TemplateHaskell, TypeApplications #-}

module Day9 (part1, part2) where

import Intcode


part1 :: IO [Int]
part1 = fmap (evalIntcode [1] . parseIntcode) readInput

part2 :: IO [Int]
part2 = fmap (evalIntcode [2] . parseIntcode) readInput

readInput :: IO String
readInput = readFile "inputs/day-9"
