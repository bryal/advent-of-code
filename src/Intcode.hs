{-# LANGUAGE LambdaCase, TemplateHaskell, TypeApplications, FlexibleContexts
           , TupleSections #-}

module Intcode
    ( parseIntcode
    , runIntcode
    , evalIntcode
    , Mem
    , stepIntcode
    , Continuation(..)
    )
where

import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative
import Lens.Micro.Platform (makeLenses, use, to, modifying, (<<%=), assign)
import Data.Sequence (Seq)
import Data.Composition
import Data.List.Split
import qualified Data.Sequence as Seq
import Data.Functor
import Data.Bifunctor


newtype Addr = Addr Int deriving (Ord, Eq)

type Pc = Int
type Mem = Seq Int

type Mode = Int

data Flag = Continue | Halt | Input Mode

newtype Continuation = Cont (Int -> ((Mem, [Int]), Maybe Continuation))

data IntcodeSt = IntcodeSt { _pc :: Pc, _base :: Addr, _mem :: Mem }
makeLenses ''IntcodeSt

type Intcode a = WriterT [Int] (State IntcodeSt) a


parseIntcode :: String -> Mem
parseIntcode s =
    let m = Seq.fromList (fmap read (splitOn "," s))
    in m Seq.>< Seq.fromFunction (8000 - Seq.length m) (const 0)

runIntcode :: [Int] -> Mem -> (Mem, [Int])
runIntcode inps pgm =
    let
        run is = \case
            ((_, outs), Just (Cont cont)) ->
                second (outs ++) (run (tail is) (cont (head is)))
            (r, Nothing) -> r
    in run inps (stepIntcode pgm)

-- | Run the Intcode program until it asks for input or halts
stepIntcode :: Mem -> ((Mem, [Int]), Maybe Continuation)
stepIntcode pgm = evalState stepIntcode' (IntcodeSt 0 (Addr 0) pgm)
  where
    stepIntcode' = do
        (r, outs) <- runWriterT multistep
        m <- use mem
        fmap ((m, outs), ) $ case r of
            Nothing -> pure Nothing
            Just mode -> do
                st <- get
                let cont inp =
                        evalState (store inp mode *> stepIntcode') st
                pure (Just (Cont cont))

evalIntcode :: [Int] -> Mem -> [Int]
evalIntcode = snd .* runIntcode

multistep :: Intcode (Maybe Mode)
multistep = step >>= \case
    Continue -> multistep
    Halt -> pure Nothing
    Input mode -> pure (Just mode)

step :: Intcode Flag
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
        99 -> pure Halt
        _ -> error "Undefined opcode"
  where
    nextInstr = fmap (\x -> (extractOpcode x, extractModes x)) next
    extractOpcode x = mod x 100
    extractModes x = map (\n -> mod (div x (10 ^ n)) 10) [2 :: Int ..]
    binop amode bmode cmode f = do
        v <- liftA2 f (getArg amode) (getArg bmode)
        store v cmode
        pure Continue
    input mode = pure (Input mode)
        -- do
        -- i <- fmap head (inp <<%= tail)
        -- store i mode
    output mode = (tell . pure =<< getArg mode) $> Continue
    jmpIf amode bmode pred' = do
        (a, b) <- liftA2 (,) (getArg amode) (getArg bmode)
        when (pred' a) (assign pc b)
        pure Continue
    adjustBase mode =
        (modifying base =<< fmap addAddr (getArg mode)) $> Continue
    getArg = \case
        1 -> next -- Immediate mode
        n -> load =<< getAddr n

store :: MonadState IntcodeSt m => Int -> Mode -> m ()
store v mode = getAddr mode >>= \(Addr i) -> modifying mem (Seq.update i v)

getAddr :: MonadState IntcodeSt m => Mode -> m Addr
getAddr = \case
    0 -> fmap Addr next -- Address mode
    2 -> relativeBase =<< next -- Relative mode
    _ -> error "getAddr"
    where relativeBase x = use (base . to (addAddr x))

next :: MonadState IntcodeSt m => m Int
next = load . Addr =<< (pc <<%= (+ 1))

load :: MonadState IntcodeSt m => Addr -> m Int
load (Addr i) = use (mem . to (flip Seq.index i))

addAddr :: Int -> Addr -> Addr
addAddr b (Addr a) = (Addr (a + b))
