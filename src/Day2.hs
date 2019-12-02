{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module Day2 (part1, part2) where

import Control.Monad.State
import Control.Applicative
import Data.Functor
import Data.List.Split
import Lens.Micro.Platform (makeLenses, use, to, modifying)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

newtype Pos = Pos { _unPos :: Word } deriving (Ord, Eq)
makeLenses ''Pos

data Instr
    = Add Pos Pos Pos
    | Mul Pos Pos Pos
    | Halt

newtype Pc = Pc { _unPc :: Pos }
makeLenses ''Pc

newtype Mem = Mem { _unMem :: Map Pos Word }
makeLenses ''Mem

data St = St { _programCount :: Pc, _memory :: Mem }
makeLenses ''St

type Eval a = State St a

-- Part 1

part1 :: IO Word
part1 = fmap (runWithArgs 12 2 . parse) readInput

readInput :: IO String
readInput = readFile "inputs/day-2"

parse :: String -> Mem
parse = Mem . Map.fromList . zip (map Pos [0 ..]) . fmap read . splitOn ","

runWithArgs :: Word -> Word -> Mem -> Word
runWithArgs a b =
    evalState (store (Pos 1) a >> store (Pos 2) b >> eval) . St (Pc (Pos 0))

eval :: Eval Word
eval = step >>= \case
    Just x -> pure x
    Nothing -> eval

step :: Eval (Maybe Word)
step = do
    nextInstr >>= \case
        Add i j dst -> (store dst =<< liftA2 (+) (load i) (load j)) $> Nothing
        Mul i j dst -> (store dst =<< liftA2 (*) (load i) (load j)) $> Nothing
        Halt -> fmap Just (load (Pos 0))

nextInstr :: Eval Instr
nextInstr = do
    St (Pc pc) (Mem mem) <- get
    case (mem Map.! pc) of
        1 -> binop Add pc mem
        2 -> binop Mul pc mem
        99 -> incrPc 1 $> Halt
        _ -> error "Undefined opcode"
  where
    binop f (Pos pc) mem =
        let
            i = Pos (mem Map.! (Pos (pc + 1)))
            j = Pos (mem Map.! (Pos (pc + 2)))
            dst = Pos (mem Map.! (Pos (pc + 3)))
        in incrPc 4 $> f i j dst

incrPc :: Word -> Eval ()
incrPc = modifying (programCount . unPc . unPos) . (+)

store :: Pos -> Word -> Eval ()
store i = modifying (memory . unMem) . Map.insert i

load :: Pos -> Eval Word
load i = use (memory . unMem . to (Map.! i))

-- Part 2

part2 :: IO Word
part2 = fmap part2' readInput where part2' = findNounVerb . parse

findNounVerb :: Mem -> Word
findNounVerb mem = head $ do
    noun <- [0 .. 99]
    verb <- [0 .. 99]
    if runWithArgs noun verb mem == expectedOutput
        then [100 * noun + verb]
        else []

expectedOutput :: Word
expectedOutput = 19690720
