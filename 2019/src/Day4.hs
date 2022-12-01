module Day4 (part1, part2) where

part1 :: Int
part1 = nValids (\s -> increasing s && any ((>= 2) . length) (chunks s))

part2 :: Int
part2 = nValids (\s -> increasing s && any ((== 2) . length) (chunks s))

increasing :: String -> Bool
increasing s = all (uncurry (<=)) (zip s (tail s))

chunks :: String -> [String]
chunks s = snd (foldl f (head s, [[head s]]) (tail s))
  where
    f (c0, ks) c1 =
        if c1 == c0 then (c0, (c0 : head ks) : tail ks) else (c1, [c1] : ks)

nValids :: (String -> Bool) -> Int
nValids pred = length (filter (pred . show) (uncurry enumFromTo input))

input :: (Int, Int)
input = (178416, 676461)
