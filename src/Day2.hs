{-# LANGUAGE LambdaCase #-}

module Day2 (part1, part2) where

import Data.List.Split
import Numeric.Natural

type Pos = Natural

data Instr
    = Add Pos Pos Pos
    | Mul Pos Pos Pos
    | Halt

-- Program counter
type Pc = Natural

type Mem = [Natural]

-- Part 1

part1 :: IO Natural
part1 = fmap part1' readInput where part1' = evalPgm 12 2 . parse

readInput :: IO String
readInput = readFile "inputs/day-2"

parse :: String -> Mem
parse = map read . splitOn ","

evalPgm :: Natural -> Natural -> Mem -> Natural
evalPgm a b = eval 0 . store a 1 . store b 2

eval :: Pc -> Mem -> Natural
eval pc mem =
    let
        instr = getInstr pc mem
        pc' = pc + 4
    in case evalInstr mem instr of
        Left mem' -> eval pc' mem'
        Right r -> r

getInstr :: Pc -> Mem -> Instr
getInstr pc mem = case (drop (fromIntegral pc) mem) of
    1 : ia : ib : dest : _ -> Add ia ib dest
    2 : ia : ib : dest : _ -> Mul ia ib dest
    99 : _ -> Halt
    mem' -> error $ "getInstr: " ++ show (take 4 mem') ++ "..."

evalInstr :: Mem -> Instr -> Either Mem Natural
evalInstr mem = \case
    Add ia ib dest ->
        Left $ store (lookupMem ia mem + lookupMem ib mem) dest mem
    Mul ia ib dest ->
        Left $ store (lookupMem ia mem * lookupMem ib mem) dest mem
    Halt -> Right (lookupMem 0 mem)

store :: Natural -> Pos -> Mem -> Mem
store x i mem = case (i, mem) of
    (0, _ : mem') -> x : mem'
    (_, a : mem') -> a : store x (i - 1) mem'
    _ -> error "store"

lookupMem :: Pos -> Mem -> Natural
lookupMem i = (!! fromIntegral i)

-- Part 2

part2 :: IO Natural
part2 = fmap part2' readInput where part2' = findNounVerb . parse

findNounVerb :: Mem -> Natural
findNounVerb mem = head $ do
    noun <- [0 .. 99]
    verb <- [0 .. 99]
    if evalPgm noun verb mem == expectedOutput then [100 * noun + verb] else []

expectedOutput :: Natural
expectedOutput = 19690720
