{-# LANGUAGE LambdaCase, TemplateHaskell, TypeApplications #-}

module Day9 (part1, part2) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative
import Data.List.Split
import Lens.Micro.Platform (makeLenses, use, to, modifying, (<<%=), assign)
import Data.Sequence (Seq)
import Data.Composition
import qualified Data.Sequence as Seq

newtype Addr = Addr Int deriving (Ord, Eq)

type Pc = Int
type Mem = Seq Int

data St = St { _pc :: Pc, _base :: Addr, _inp :: [Int], _mem :: Mem }
makeLenses ''St

type Eval a = StateT St (Writer [Int]) a

part1 :: IO [Int]
part1 = fmap (run [1] . parse) readInput

part2 :: IO [Int]
part2 = fmap (run [2] . parse) readInput

readInput :: IO String
readInput = readFile "inputs/day-9"

parse :: String -> Mem
parse s =
    let m = Seq.fromList (fmap read (splitOn "," s))
    in m Seq.>< Seq.fromFunction (8000 - Seq.length m) (const 0)

run :: [Int] -> Mem -> [Int]
run = execWriter . evalStateT eval .* St 0 (Addr 0)

eval :: Eval ()
eval = step >>= \case
    True -> pure ()
    False -> eval

step :: Eval Bool
step = do
    (opcode, modes) <- nextInstr
    let binop' = binop (modes !! 0) (modes !! 1) (modes !! 2)
    let jmpIf' = jmpIf (modes !! 0) (modes !! 1)
    case opcode of
        1 -> binop' (+)
        2 -> binop' (*)
        3 -> input (modes !! 0)
        4 -> output (modes !! 0)
        5 -> jmpIf' (/= 0)
        6 -> jmpIf' (== 0)
        7 -> binop' (fromEnum .* (<))
        8 -> binop' (fromEnum .* (==))
        9 -> adjustBase (modes !! 0)
        99 -> pure ()
        _ -> error "Undefined opcode"
    pure (if opcode == 99 then True else False)
  where
    nextInstr = fmap (\x -> (extractOpcode x, extractModes x)) next
    extractOpcode x = mod x 100
    extractModes x = map (\n -> mod (div x (10 ^ n)) 10) [2 :: Int ..]
    next = load . Addr =<< (pc <<%= (+ 1))
    binop amode bmode cmode f = do
        v <- liftA2 f (getArg amode) (getArg bmode)
        store v cmode
    input mode = do
        i <- fmap head (inp <<%= tail)
        store i mode
    output mode = tell . pure =<< getArg mode
    jmpIf amode bmode pred = do
        (a, b) <- liftA2 (,) (getArg amode) (getArg bmode)
        when (pred a) (assign pc b)
    adjustBase mode = modifying base =<< fmap addAddr (getArg mode)
    store v mode =
        getAddr mode >>= \(Addr i) -> modifying mem (Seq.update i v)
    load (Addr i) = use (mem . to (flip Seq.index i)) :: Eval Int
    getArg = \case
        1 -> next -- Immediate mode
        n -> load =<< getAddr n
    getAddr = \case
        0 -> fmap Addr next -- Address mode
        2 -> relativeBase =<< next -- Relative mode
        _ -> error "getAddr"
    relativeBase x = use (base . to (addAddr x)) :: Eval Addr
    addAddr b (Addr a) = (Addr (a + b))
