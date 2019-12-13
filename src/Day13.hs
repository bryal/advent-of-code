{-# LANGUAGE LambdaCase #-}

module Day13 (part1, part2) where

import Data.List.Split
import qualified Data.Sequence as Seq
import Data.Bifunctor

import Lib
import Intcode


part1 :: IO Int
part1 = fmap
    (length
    . filter (== 2)
    . everyNth 3
    . drop 2
    . evalIntcode []
    . parseIntcode
    )
    readInput

part2 :: IO Int
part2 = fmap (beatGame . parseIntcode) readInput
  where
    beatGame pgm =
        let
            os = evalIntcode is (insertCoins pgm)
            insertCoins = Seq.update 0 2
            tiles = map head3 (chunksOf 3 os)
            (score, is) = play tiles
            play = play' (0, 0, 0)
            play' (bx, px, s) = \case
                (-1, 0, s') : ts -> play' (bx, px, s') ts
                (px', _, 3) : ts -> play' (bx, px', s) ts
                (bx', _, 4) : ts ->
                    let
                        i = calcMove bx' px
                        result = play' (bx', px, s) ts
                    in second (i :) result
                _ : ts -> play' (bx, px, s) ts
                [] -> (s, [])
            calcMove bx px =
                if bx > px then 1 else if bx == px then 0 else (-1)
        in score

readInput :: IO String
readInput = readFile "inputs/day-13"
