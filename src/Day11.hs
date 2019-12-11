{-# LANGUAGE LambdaCase, TemplateHaskell, TypeApplications #-}

module Day11 (part1, part2) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Applicative
import Data.List.Split
import Lens.Micro.Platform (makeLenses, use, to, modifying, (<<%=), assign)
import Data.Sequence (Seq)
import Data.Composition
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Data.Set (Set)

import Lib

newtype Addr = Addr Int deriving (Ord, Eq)

type Pc = Int
type Mem = Seq Int

data St = St { _pc :: Pc, _base :: Addr, _inp :: [Int], _mem :: Mem }
makeLenses ''St

type Eval a = StateT St (Writer [Int]) a

type Pos = (Int, Int)

part1 :: IO Int
part1 = fmap (fst . paintShip Set.empty . parse) readInput

part2 :: IO ()
part2 =
    putStrLn
        . drawHull
        . snd
        . paintShip (Set.singleton (0, 0))
        . parse
        =<< readInput
  where
    drawHull h =
        let
            h' = Set.toList h
            (xs, ys) = (map fst h', map snd h')
            (bot, top) = (minimum ys, maximum ys)
            (left, right) = (minimum xs, maximum xs)
        in
            unlines
            $ flip map (reverse [bot .. top])
            $ \y -> flip map [left .. right]
                $ \x -> if Set.member (x, y) h then '█' else ' '

paintShip :: Set Pos -> Mem -> (Int, Set Pos)
paintShip initHull pgm =
    let
        os = run is pgm
        (colors, turns) = unzip (map head2 (chunksOf 2 os))
        up = (0, 1)
        (poss, _) = unzip (scanl move ((0, 0), up) turns)
        hulls = scanl paint initHull (zip colors poss)
        is = zipWith (fromEnum .* Set.member) poss hulls
        nPanelsVisited = Set.size (Set.fromList poss)
    in (nPanelsVisited, last hulls)
  where
    move :: ((Int, Int), (Int, Int)) -> Int -> ((Int, Int), (Int, Int))
    move (pos, dir) = \case
        0 -> let dir' = turnLeft dir in (addPos pos dir', dir')
        _ -> let dir' = turnRight dir in (addPos pos dir', dir')
    turnLeft (dx, dy) = (-dy, dx)
    turnRight (dx, dy) = (dy, -dx)
    addPos (x, y) (dx, dy) = (x + dx, y + dy)
    paint hull (c, pos) = case c of
        0 -> Set.delete pos hull
        _ -> Set.insert pos hull

readInput :: IO String
readInput = readFile "inputs/day-11"

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
