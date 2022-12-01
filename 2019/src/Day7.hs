{-# LANGUAGE LambdaCase, TemplateHaskell, TypeApplications #-}

module Day7 (part1, part2) where

import Data.Function
import Data.List.Split
import Data.List
import qualified Data.Sequence as Seq

import Intcode


part1 :: IO Int
part1 = fmap
    (\pgm -> maximum (map (amp pgm) (permutations [0 .. 4])))
    (fmap parse readInput)
  where
    amp ampPgm = foldl' (&) 0
        . map (\s -> \inp -> head (evalIntcode [s, inp] ampPgm))

part2 :: IO Int
part2 = fmap
    (\pgm -> maximum (map (amp pgm) (permutations [5 .. 9])))
    (fmap parse readInput)
  where
    amp ampPgm phaseSettings =
        let
            ampStage s inps = evalIntcode (s : inps) ampPgm
            outs = foldl (&) (0 : init outs) (map ampStage phaseSettings)
        in last outs

readInput :: IO String
readInput = readFile "inputs/day-7"

parse :: String -> Mem
parse = Seq.fromList . fmap read . splitOn ","
