{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module Day2 (part1, part2) where

import Control.Monad.State
import Control.Applicative
import Data.Functor
import Data.List.Split
import Lens.Micro.Platform (makeLenses, use, to, modifying)
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

newtype Pos = Pos Word deriving (Ord, Eq)

data Instr
    = Add Pos Pos Pos
    | Mul Pos Pos Pos
    | Halt

newtype Pc = Pc { _unPc :: Word }
makeLenses ''Pc

newtype Mem = Mem { _unMem :: Seq Word }
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
parse = Mem . Seq.fromList . fmap read . splitOn ","

runWithArgs :: Word -> Word -> Mem -> Word
runWithArgs a b =
    evalState (store (Pos 1) a >> store (Pos 2) b >> eval) . St (Pc 0)

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
    let pc' = fromIntegral pc
    case (Seq.index mem pc') of
        1 -> binop Add pc' mem
        2 -> binop Mul pc' mem
        99 -> incrPc 1 $> Halt
        _ -> error "Undefined opcode"
  where
    binop f pc mem =
        let
            i = Pos (Seq.index mem (pc + 1))
            j = Pos (Seq.index mem (pc + 2))
            dst = Pos (Seq.index mem (pc + 3))
        in incrPc 4 $> f i j dst

incrPc :: Word -> Eval ()
incrPc = modifying (programCount . unPc) . (+)

store :: Pos -> Word -> Eval ()
store (Pos i) = modifying (memory . unMem) . Seq.update (fromIntegral i)

load :: Pos -> Eval Word
load (Pos i) = use (memory . unMem . to (flip Seq.index (fromIntegral i)))

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
