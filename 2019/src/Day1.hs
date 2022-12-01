{-# LANGUAGE LambdaCase #-}

-- | Day 1: The Tyranny of the Rocket Equation
module Day1 (part1, part2) where

-- Part 1

part1 :: IO Word
part1 = fmap part1' readInput
    where part1' = sum . map (directFuelToLaunchMass . read) . lines

readInput :: IO String
readInput = readFile "inputs/day-1"

directFuelToLaunchMass :: Word -> Word
directFuelToLaunchMass m = saturatingSub (div m 3) 2

saturatingSub :: Word -> Word -> Word
saturatingSub a b = if a < b then 0 else a - b

-- Part 2

part2 :: IO Word
part2 = fmap part2' readInput
    where part2' = sum . map (fuelToLaunchMass . read) . lines

fuelToLaunchMass :: Word -> Word
fuelToLaunchMass = \case
    0 -> 0
    m -> let f = directFuelToLaunchMass m in f + fuelToLaunchMass f
