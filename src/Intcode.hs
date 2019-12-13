{-# LANGUAGE LambdaCase, TemplateHaskell, TypeApplications #-}

module Intcode (parseIntcode, runIntcode, evalIntcode, Mem) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative
import Lens.Micro.Platform (makeLenses, use, to, modifying, (<<%=), assign)
import Data.Sequence (Seq)
import Data.Composition
import Data.List.Split
import qualified Data.Sequence as Seq


newtype Addr = Addr Int deriving (Ord, Eq)

type Pc = Int
type Mem = Seq Int

data St = St { _pc :: Pc, _base :: Addr, _inp :: [Int], _mem :: Mem }
makeLenses ''St

type Intcode a = StateT St (Writer [Int]) a


parseIntcode :: String -> Mem
parseIntcode s =
    let m = Seq.fromList (fmap read (splitOn "," s))
    in m Seq.>< Seq.fromFunction (8000 - Seq.length m) (const 0)

runIntcode :: [Int] -> Mem -> (Mem, [Int])
runIntcode = runWriter . fmap _mem . execStateT multistep .* St 0 (Addr 0)

evalIntcode :: [Int] -> Mem -> [Int]
evalIntcode = snd .* runIntcode

multistep :: Intcode ()
multistep = step >>= \case
    True -> pure ()
    False -> multistep

step :: Intcode Bool
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
    load (Addr i) = use (mem . to (flip Seq.index i)) :: Intcode Int
    getArg = \case
        1 -> next -- Immediate mode
        n -> load =<< getAddr n
    getAddr = \case
        0 -> fmap Addr next -- Address mode
        2 -> relativeBase =<< next -- Relative mode
        _ -> error "getAddr"
    relativeBase x = use (base . to (addAddr x)) :: Intcode Addr
    addAddr b (Addr a) = (Addr (a + b))
