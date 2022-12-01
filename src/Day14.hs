{-# LANGUAGE NumDecimals #-}

module Day14 (part1, part2) where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative
import Data.Foldable
import Data.Bifunctor

import Lib
import Parse


type Chemical = String
type Reactions = Map Chemical (Int, [(Int, Chemical)])


part1 :: IO Int
part1 = fmap (produce 1 "FUEL" . parse) readInput

part2 :: IO Int
part2 = do
    inp <- readInput
    let reacts = parse inp
        produceFuel n = produce n "FUEL" reacts
        collectedOres = 1e12
    -- Leftover chemicals from a previous fuel production could make the next
    -- batch cheaper, so we can't simply produce 1 fuel from scratch and
    -- calculate how many of those we can afford. Instead, calculate a lower
    -- bound and do a binary search.
    let maxOresPerFuel = produceFuel 1
        lowerBound = div collectedOres maxOresPerFuel
    pure $ bsearchIntMax
        (\n -> produceFuel n < collectedOres)
        lowerBound
        collectedOres

-- | Returns how much ORE it costs to produce the desired quantity and chemical
--   from scratch
produce :: Int -> Chemical -> Reactions -> Int
produce num chemical reacts = fst
    (produce' num chemical (fmap (const 0) reacts))
  where
    produce' n chem store = case (n, chem) of
        (0, _) -> (0, store)
        (_, "ORE") -> (n, store)
        _ ->
            let
                avail = store Map.! chem
                avail' = max 0 (avail - n)
                fromStore = avail - avail'
                n' = n - fromStore
                (perReact, deps) = reacts Map.! chem
                nReacts = div (n' - 1) perReact + 1
                leftOver = perReact * nReacts - n'
                store' = Map.insert chem (avail' + leftOver) store
            in foldl'
                (\(o, s) (m, c) -> first (o +) (produce' (m * nReacts) c s))
                (0, store')
                deps

parse :: String -> Reactions
parse = fmap Map.fromList (applyParser (sepEndBy reaction eol))
  where
    reaction = do
        inputs <- sepBy amountChem (string ", ")
        string " => "
        (n, c) <- amountChem
        pure (c, (n, inputs))
    amountChem = liftA2 (,) int (space1 *> word)

readInput :: IO String
readInput = readFile "inputs/day-14"
