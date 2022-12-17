module Help where

mapHead :: (a -> a) -> [a] -> [a]
mapHead f xs = f (head xs) : tail xs
