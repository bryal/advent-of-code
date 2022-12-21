{-# LANGUAGE TupleSections, LambdaCase #-}
module Day18 where

import Control.Arrow
import qualified Data.Map as Map
import qualified Data.Set as Set

data Vec3 = V3
    { vx :: Int
    , vy :: Int
    , vz :: Int
    }
    deriving (Eq, Ord, Show)
type Cube = Vec3
data Side = Cube `Interfacing` Cube
    deriving (Eq, Ord)

c1 `interfacing` c2 = min c1 c2 `Interfacing` max c1 c2

day18 = do
    cubes <- map parseCube . lines <$> readFile "../inputs/day18.txt"
    putStrLn "Part 1:"
    print (airFacingSides cubes)
    putStrLn "Part 2:"
    print (airFacingSides (cubes ++ trappedCubes cubes))

trappedCubes cubes =
    let
        (xmin, xmax) = (minimum &&& maximum) (map vx cubes)
        (ymin, ymax) = (minimum &&& maximum) (map vy cubes)
        (zmin, zmax) = (minimum &&& maximum) (map vz cubes)
        lava = Set.fromList cubes
        inbounds (V3 x y z) =
            (x >= xmin - 1 && x <= xmax + 1)
                && (y >= ymin - 1 && y <= ymax + 1)
                && (z >= zmin - 1 && z <= zmax + 1)
        explore seen = \case
            [] -> seen
            (c : nexts)
                | not (inbounds c) || Set.member c seen || Set.member c lava -> explore
                    seen
                    nexts
                | otherwise -> explore (Set.insert c seen) (neighs c ++ nexts)
        externalAir = explore Set.empty [V3 (xmin - 1) (ymin - 1) (zmin - 1)]
        all = Set.fromList
            [ V3 x y z | x <- [xmin .. xmax], y <- [ymin .. ymax], z <- [zmin .. zmax] ]
    in
        Set.toList $ all `Set.difference` Set.union lava externalAir

airFacingSides cubes =
    let sidesCount = Map.fromListWith (+) (map (, 1 :: Int) (sides =<< cubes))
    in  length (filter ((== 1) . snd) (Map.toList sidesCount))

sides cube = map (cube `interfacing`) (neighs cube)

neighs cube =
    map (v3add cube) [V3 1 0 0, V3 (-1) 0 0, V3 0 1 0, V3 0 (-1) 0, V3 0 0 1, V3 0 0 (-1)]

v3add (V3 x1 y1 z1) (V3 x2 y2 z2) = V3 (x1 + x2) (y1 + y2) (z1 + z2)

parseCube s0 =
    let (x, s1) = span (/= ',') s0
        (y, s2) = span (/= ',') (drop 1 s1)
        (z, _) = span (/= ',') (drop 1 s2)
    in  V3 (read x) (read y) (read z)
