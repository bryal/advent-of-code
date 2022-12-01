{-# LANGUAGE LambdaCase, TemplateHaskell, TypeApplications #-}

module Day11 (part1, part2) where

import Data.List.Split
import Data.Composition
import qualified Data.Set as Set
import Data.Set (Set)

import Lib
import Vec
import Intcode


type Pos = Vec2 Int


part1 :: IO Int
part1 = fmap (fst . paintShip Set.empty . parseIntcode) readInput

part2 :: IO ()
part2 =
    putStrLn
        . drawHull
        . snd
        . paintShip (Set.singleton (Vec2 0 0))
        . parseIntcode
        =<< readInput
  where
    drawHull :: Set Pos -> String
    drawHull h =
        let
            Vec2 xs ys = sequence (Set.toList h)
            (bot, top) = (minimum ys, maximum ys)
            (left, right) = (minimum xs, maximum xs)
        in unlines $ flip map (reverse [bot .. top]) $ \y ->
            flip map [left .. right]
                $ \x -> if Set.member (Vec2 x y) h then '█' else ' '

paintShip :: Set Pos -> Mem -> (Int, Set Pos)
paintShip initHull pgm =
    let
        os = evalIntcode is pgm
        (colors, turns) = unzip (map head2 (chunksOf 2 os))
        up = Vec2 0 1
        (poss, _) = unzip (scanl move (Vec2 0 0, up) turns)
        hulls = scanl paint initHull (zip colors poss)
        is = zipWith (fromEnum .* Set.member) poss hulls
        nPanelsVisited = Set.size (Set.fromList poss)
    in (nPanelsVisited, last hulls)
  where
    move :: (Pos, Vec2 Int) -> Int -> (Pos, Vec2 Int)
    move (pos, dir) = \case
        0 -> let dir' = turnLeft dir in (pos + dir', dir')
        _ -> let dir' = turnRight dir in (pos + dir', dir')
    turnLeft (Vec2 dx dy) = Vec2 (-dy) dx
    turnRight (Vec2 dx dy) = Vec2 dy (-dx)
    paint hull (c, pos) = case c of
        0 -> Set.delete pos hull
        _ -> Set.insert pos hull

readInput :: IO String
readInput = readFile "inputs/day-11"
