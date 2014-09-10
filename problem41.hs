import Data.Numbers.Primes
import Data.List

res = maximum $ filter (isPrime) $ candidates where
    num = permutations "1234567"
    stringToInt = (\n -> read n::Int) 
    candidates = map stringToInt num 
     

---------------
digits = "123456789"
isNPandigital n = (==(take n digits)) . sort . show
nPandigitalPrimes n = [p | p <- takeWhile (<(10^n)) primes, isNPandigital n p]


(*|) a b = a * 10 + b
undigits xs = foldl (*|) 0 xs
-- 1 to 9 pandigitals
pandigitals = concat $ map (\n -> map undigits (permutations [1..n])) [1..9]

test  = print $ maximum $ filter isPrime pandigitals



-----------------
isNPandig :: Int -> Int -> Bool
isNPandig n num = (sort.show)  num == concatMap show  [1..n]

combinations :: Int -> [a] -> [[a]]
combinations k xs = combinations' (length xs) k xs
  where combinations' n k' l@(y:ys)
          | k' == 0   = [[]]
          | k' >= n   = [l]
          | null l    = []
          | otherwise = map (y :) (combinations' (n - 1) (k' - 1) ys) ++ combinations' (n - 1) k' ys 

f1 = map (\n -> read n::Int) $ concatMap permutations $ combinations 1 "123456789"
