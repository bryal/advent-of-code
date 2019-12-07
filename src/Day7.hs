{-# LANGUAGE LambdaCase, TemplateHaskell, TypeApplications #-}

module Day7 (part1, part2) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative
import Data.Function
import Data.List.Split
import Data.List
import Lens.Micro.Platform (makeLenses, use, to, modifying, (<<%=), assign)
import Data.Sequence (Seq)
import Data.Composition
import qualified Data.Sequence as Seq

newtype Addr = Addr Int deriving (Ord, Eq)

type Pc = Int
type Mem = Seq Int

data St = St { _pc :: Pc, _inp :: [Int], _mem :: Mem }
makeLenses ''St

type Eval a = StateT St (Writer [Int]) a

part1 :: IO Int
part1 = fmap
    (\pgm -> maximum (map (amp pgm) (permutations [0 .. 4])))
    (fmap parse readInput)
  where
    amp ampPgm =
        foldl' (&) 0 . map (\s -> \inp -> head (run [s, inp] ampPgm))

part2 :: IO Int
part2 = fmap
    (\pgm -> maximum (map (amp pgm) (permutations [5 .. 9])))
    (fmap parse readInput)
  where
    amp ampPgm phaseSettings =
        let
            ampStage s inps = run (s : inps) ampPgm
            outs = foldl (&) (0 : init outs) (map ampStage phaseSettings)
        in last outs

readInput :: IO String
readInput = readFile "inputs/day-7"

parse :: String -> Mem
parse = Seq.fromList . fmap read . splitOn ","

run :: [Int] -> Mem -> [Int]
run = execWriter . evalStateT eval .* St 0

eval :: Eval ()
eval = step >>= \case
    True -> pure ()
    False -> eval

step :: Eval Bool
step = do
    (opcode, modes) <- nextInstr
    let binop' = binop (modes !! 0) (modes !! 1)
    let jmpIf' = jmpIf (modes !! 0) (modes !! 1)
    case opcode of
        1 -> binop' (+)
        2 -> binop' (*)
        3 -> input
        4 -> output (modes !! 0)
        5 -> jmpIf' (/= 0)
        6 -> jmpIf' (== 0)
        7 -> binop' (fromEnum .* (<))
        8 -> binop' (fromEnum .* (==))
        99 -> pure ()
        _ -> error "Undefined opcode"
    pure (if opcode == 99 then True else False)
  where
    nextInstr = fmap (\x -> (extractOpcode x, extractModes x)) next
    extractOpcode x = mod x 100
    extractModes x = map (\n -> mod (div x (10 ^ n)) 10) [2 :: Int ..]
    next = load . Addr =<< (pc <<%= (+ 1))
    binop amode bmode f = do
        v <- liftA2 f (getArg amode) (getArg bmode)
        dst <- nextAddr
        store dst v
    input = do
        i <- fmap head (inp <<%= tail)
        dst <- nextAddr
        store dst i
    output mode = tell . pure =<< getArg mode
    jmpIf amode bmode pred = do
        (a, b) <- liftA2 (,) (getArg amode) (getArg bmode)
        when (pred a) (assign pc b)
    getArg = \case
        0 -> load =<< nextAddr -- Address mode
        1 -> next -- Immediate mode
        _ -> error "Undefined mode"
    nextAddr = fmap Addr next

store :: Addr -> Int -> Eval ()
store (Addr i) = modifying mem . Seq.update i

load :: Addr -> Eval Int
load (Addr i) = use (mem . to (flip Seq.index i))
