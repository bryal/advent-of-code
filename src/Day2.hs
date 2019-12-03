{-# LANGUAGE LambdaCase, TemplateHaskell #-}

module Day2 (part1, part2) where

import Control.Monad.State
import Control.Applicative
import Data.Functor
import Data.List.Split
import Lens.Micro.Platform (makeLenses, use, to, modifying, (<<%=))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

newtype Addr = Addr Word deriving (Ord, Eq)

type Pc = Word
type Mem = Seq Word

data St = St { _pc :: Pc, _mem :: Mem }
makeLenses ''St

type Eval a = State St a

-- Part 1

part1 :: IO Word
part1 = fmap (runWithArgs 12 2 . parse) readInput

readInput :: IO String
readInput = readFile "inputs/day-2"

parse :: String -> Mem
parse = Seq.fromList . fmap read . splitOn ","

runWithArgs :: Word -> Word -> Mem -> Word
runWithArgs a b =
    evalState (store (Addr 1) a >> store (Addr 2) b >> eval) . St 0

eval :: Eval Word
eval = step >>= \case
    Just x -> pure x
    Nothing -> eval

step :: Eval (Maybe Word)
step = next >>= \case
    1 -> binop (+)
    2 -> binop (*)
    99 -> fmap Just (load (Addr 0))
    _ -> error "Undefined opcode"
  where
    next = load . Addr =<< (pc <<%= (+ 1))
    binop f = do
        v <- liftA2 f getArg getArg
        dst <- nextAddr
        store dst v $> Nothing
    getArg = load =<< nextAddr
    nextAddr = fmap Addr next

store :: Addr -> Word -> Eval ()
store (Addr i) = modifying mem . Seq.update (fromIntegral i)

load :: Addr -> Eval Word
load (Addr i) = use (mem . to (flip Seq.index (fromIntegral i)))

-- Part 2

part2 :: IO Word
part2 = fmap part2' readInput where part2' = findNounVerb . parse

findNounVerb :: Mem -> Word
findNounVerb m = head $ do
    noun <- [0 .. 99]
    verb <- [0 .. 99]
    if runWithArgs noun verb m == expectedOutput
        then [100 * noun + verb]
        else []

expectedOutput :: Word
expectedOutput = 19690720
