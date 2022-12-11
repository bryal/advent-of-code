module Day11 where

import Control.Monad.State.Strict
import Data.Foldable
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Void
import Text.Megaparsec hiding (State)
import Text.Megaparsec.Char hiding (eol)
import Text.Megaparsec.Char.Lexer hiding (space)
import Prelude hiding (round)

type Parser a = Parsec Void String a
type Worry = Int
data Monkey = Monkey
    { items :: Seq Worry
    , operation :: Worry -> Worry
    , test :: (Worry, Int, Int)
    }

day11 :: IO ()
day11 = do
    input <- readFile "../inputs/day11.txt"
    let ms = either (error . errorBundlePretty) id (parse pMonkeys "input" input)
        lcm' = foldr1 lcm (map (\(Monkey _ _ (denom, _, _)) -> denom) (toList ms))
        inspections0 = fmap (const 0) ms
        rounds1 = rounds (`div` 3)
        rounds2 = rounds (`mod` lcm')
    putStrLn $ "Part 1: " ++ show (business (execState (rounds1 20 ms) inspections0))
    putStrLn $ "Part 2: " ++ show (business (execState (rounds2 10000 ms) inspections0))

business :: Seq Int -> Int
business = product . take 2 . toList . Seq.reverse . Seq.sort

rounds :: (Int -> Int) -> Int -> Seq Monkey -> State (Seq Int) (Seq Monkey)
rounds relief nrounds ms = foldl' (>>=) (pure ms) (replicate nrounds (round relief))

round :: (Int -> Int) -> Seq Monkey -> State (Seq Int) (Seq Monkey)
round relief ms = foldlM (turn relief) ms [0 .. length ms - 1]

turn :: (Int -> Int) -> Seq Monkey -> Int -> State (Seq Int) (Seq Monkey)
turn relief ms i = do
    let m = Seq.index ms i
        ms' = Seq.update i (m { items = empty }) ms
    foldlM
        (\ms' worry -> do
            let worry' = relief (operation m worry)
                (denom, j1, j2) = test m
                j = if worry' `mod` denom == 0 then j1 else j2
            modify (Seq.adjust (+ 1) i)
            pure $ Seq.adjust (\m2 -> m2 { items = items m2 Seq.:|> worry' }) j ms'
        )
        ms'
        (items m)

pMonkeys :: Parser (Seq Monkey)
pMonkeys = Seq.fromList <$> many pMonkey

pMonkey :: Parser Monkey
pMonkey = do
    _i <- string "Monkey " *> decimal <* char ':' <* space1 :: Parser Int
    items <- string "Starting items: " *> sepBy decimal (char ',' *> space)
    op <- space1 *> string "Operation: " *> pOperation
    test <- string "Test: " *> pTest
    pure (Monkey (Seq.fromList items) op test)

pOperation :: Parser (Worry -> Worry)
pOperation = do
    op <- string "new = old " *> (((+) <$ char '+') <|> ((*) <$ char '*'))
    b <- space *> (fmap Just decimal <|> (Nothing <$ string "old")) <* space
    pure $ \a -> case b of
        Just b -> a `op` b
        Nothing -> a `op` a

pTest :: Parser (Worry, Int, Int)
pTest = do
    denom <- string "divisible by " *> decimal <* space1
    conseq <- string "If true: throw to monkey " *> decimal <* space1
    alt <- string "If false: throw to monkey " *> decimal <* space
    pure (denom, conseq, alt)
