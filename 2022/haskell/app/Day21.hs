{-# LANGUAGE LambdaCase #-}
module Day21 where

import Data.Char
import qualified Data.Map as Map
import Data.Ratio

data Expr
    = Var
    | Val Rational
    | Op Char Expr Expr
    deriving Eq

instance Show Expr where
    show = \case
        Var -> "x"
        Val v | denominator v == 1 -> show (numerator v)
              | otherwise -> "(" ++ show v ++ ")"
        Op op x y -> "(" ++ show x ++ " " ++ op : " " ++ show y ++ ")"

day21 = do
    (root, humn) <- parse <$> readFile "../inputs/day21.txt"
    putStrLn $ "Part 1: " ++ show (part1 humn root)
    putStrLn $ "Part 2: " ++ show (part2 root)
  where
    part1 humn = fromVal . evalAt humn
    part2 = fromVal . evalAt (Val 0) . inverse Var . evalAt Var . rootDiff

fromVal = \case
    Val a -> a
    _ -> undefined

rootDiff = \case
    Op _ a b -> Op '-' a b
    _ -> undefined

inverse y = \case
    _x@Var -> y
    Val _ -> undefined
    Op '+' a@(Val _) b -> inverse (Op '-' y a) b
    Op '+' a b@(Val _) -> inverse (Op '-' y b) a
    Op '-' a@(Val _) b -> inverse (Op '-' a y) b
    Op '-' a b@(Val _) -> inverse (Op '+' b y) a
    Op '*' a@(Val _) b -> inverse (Op '/' y a) b
    Op '*' a b@(Val _) -> inverse (Op '/' y b) a
    Op '/' a@(Val _) b -> inverse (Op '/' a y) b
    Op '/' a b@(Val _) -> inverse (Op '*' b y) a
    Op{} -> undefined

evalAt x = \case
    Var -> x
    Val a -> Val a
    Op op a b -> case (evalAt x a, evalAt x b) of
        (Val a', Val b') -> Val (opf op a' b')
        (a', b') -> Op op a' b'

opf = \case
    '+' -> (+)
    '-' -> (-)
    '*' -> (*)
    _ -> (/)

parse s = (defs Map.! "root", defs Map.! "humn")
  where
    defs = Map.fromList (map pDef (lines s))
    pDef s = (take 4 s, pExpr (drop 6 s))
    pExpr s = if isDigit (head s)
        then Val (fromIntegral (read s :: Int))
        else Op (s !! 5) (pVar (take 4 s)) (pVar (take 4 (drop 7 s)))
    pVar = \case
        "humn" -> Var
        s -> defs Map.! s
