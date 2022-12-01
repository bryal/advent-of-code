{-# LANGUAGE LambdaCase #-}

module Day8 (part1, part2) where

import Data.List.Split
import Data.List

import Lib


part1 :: IO Int
part1 = fmap
    (snd . minimumOn fst . map f . chunksOf (width * height))
    readInput
  where
    f cs = (nDigs '0' cs, nDigs '1' cs * nDigs '2' cs)
    nDigs c cs = length (filter (== c) cs)

part2 :: IO ()
part2 =
    readInput
        >>= putStrLn
        . unlines
        . chunksOf width
        . map stack
        . transpose
        . chunksOf (width * height)
  where
    stack = \case
        '0' : _ -> ' '
        '1' : _ -> '█'
        '2' : cs -> stack cs
        _ -> error "stack"

width, height :: Int
width = 25
height = 6

readInput :: IO String
readInput = readFile "inputs/day-8"
