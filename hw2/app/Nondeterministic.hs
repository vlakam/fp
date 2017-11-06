module Nondetermenistic
    (  bin
    ,  combinations
    ,  permutations
    ) where

bin :: Int -> [[Int]]
bin 0 = [[]]
bin n = bin (n - 1) >>= \b -> [0:b, 1:b]

combinations :: Int -> Int -> [[Int]]
combinations n k = [[]]

permutations :: [a] -> [[a]]
permutations []     = return []
permutations (x:xs) = permutations xs >>= ins x
  where
    ins :: a -> [a] -> [[a]]
    ins x []     = [[x]]
    ins x (y:ys) = (x:y:ys) ++ map (y:) (ins x ys)
