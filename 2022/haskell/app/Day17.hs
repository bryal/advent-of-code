module Day17 where

import Help

import Data.Maybe
import Data.List
import Data.Set (Set)
import qualified Data.Set as Set

type Rock = (Int, Int)
type Shape = [Rock]

shapes :: [Shape]
shapes =
    [ [(0, 0), (1, 0), (2, 0), (3, 0)]
    , [(1, 2), (0, 1), (1, 1), (2, 1), (1, 0)]
    , [(2, 2), (2, 1), (0, 0), (1, 0), (2, 0)]
    , [(0, 3), (0, 2), (0, 1), (0, 0)]
    , [(0, 1), (1, 1), (0, 0), (1, 0)]
    ]

day17 :: IO ()
day17 = do
    inp <- readInput
    putStrLn "Part 1"
    print (part1 inp)
    putStrLn "Part 2"
    print (part2 1000000000000 inp)

part1 = getTall . afterStopped 2022 . simulate . head . lines

readInput = head . lines <$> readFile "../inputs/day17.txt"

getTall (_, _, t, _) = t

afterStopped n sts = fromJust $ find (\(_, nStopped, _, _) -> nStopped == n) sts

part2 n inp =
    let nwinds = length inp - 1
        -- I noticed that the rock pattern of my process repeats as the input winds cycle. I don't know
        -- /why/ this is the case, but I would assume that is the "trick" we're supposed to use.
        (_, n1, t1, _) = simulate inp !! nwinds
        (_, n2, t2, _) = simulate inp !! (nwinds * 2)
        (dn, dt) = (n2 - n1, t2 - t1)
        (_, _, trem, _) = afterStopped (n1 + mod (n - n1) dn) $ simulate inp
    in  dt * div (n - n1) dn + trem

simulate inp = scanl tick (Set.empty, 0, 0, dropShape 0 (cycle shapes)) (cycle inp)

dropShape :: Int -> [Shape] -> [Shape]
dropShape tall = mapHead (map (v2add (2, tall + 3)))

tick :: (Set Rock, Int, Int, [Shape]) -> Char -> (Set Rock, Int, Int, [Shape])
tick (_, _, _, []) _ = undefined
tick (rocks, nStopped, currentMax, shape : shapes) wind =
    let dx = if wind == '>' then 1 else -1
        shape' = move (dx, 0) shape
        shape'' = move (0, -1) shape'
        newMax = max currentMax (1 + maximum (map snd shape''))
    in  if shape'' == shape'
            then
                ( foldr Set.insert rocks shape'
                , nStopped + 1
                , newMax
                , dropShape newMax shapes
                )
            else (rocks, nStopped, currentMax, shape'' : shapes)
  where
    move delta shape =
        let shape' = map (v2add delta) shape
        in
            if any (\rock@(x, y) -> x < 0 || x > 6 || y < 0 || Set.member rock rocks)
                   shape'
                then shape
                else shape'

v2add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
