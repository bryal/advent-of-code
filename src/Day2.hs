{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module Day2 (part1, part2) where

import Data.List.Split
import qualified Data.Sequence as Seq

import Lib
import Intcode


part1 :: IO Int
part1 = fmap (runWithArgs 12 2 . parse) readInput

part2 :: IO Int
part2 = fmap (findNounVerb . parse) readInput
  where
    findNounVerb m = head $ do
        noun <- [0 .. 99]
        verb <- [0 .. 99]
        if runWithArgs noun verb m == expectedOutput
            then [100 * noun + verb]
            else []
    expectedOutput = 19690720

parse :: String -> Mem
parse = Seq.fromList . fmap read . splitOn ","

readInput :: IO String
readInput = readFile "inputs/day-2"

runWithArgs :: Int -> Int -> Mem -> Int
runWithArgs a b =
    fst . viewl' . fst . runIntcode [] . Seq.update 1 a . Seq.update 2 b
