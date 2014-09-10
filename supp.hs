import Data.List
import Control.Applicative
partition :: Int -> [a] -> [[a]]
partition n xs = partition' n xs []
  where
      partition' _ [] acc = reverse acc
      partition' n xs acc = partition' n (drop n xs) ((take n xs) : acc)

partitions [] = [[]]
partitions (x:xs) = [[x]:p | p <- partitions xs] ++ [(x:ys):yss | (ys:yss) <- partitions xs]


combinations :: Int -> [a] -> [[a]]
combinations k xs = combinations' (length xs) k xs
  where combinations' n k' l@(y:ys)
          | k' == 0   = [[]]
          | k' >= n   = [l]
          | null l    = []
          | otherwise = map (y :) (combinations' (n - 1) (k' - 1) ys) ++ combinations' (n - 1) k' ys 

isPanDig n = (length . group . sort) n == (length ) n && (length n) == 9 && nozeros n
nozeros n =  not $ '0' `elem` (show n)

f1 = map (\n -> read n::Int) $ concatMap permutations $ combinations 1 "123456789"
f2 = map (\n -> read n::Int) $ concatMap permutations $ combinations 2 "123456789"
f3 = map (\n -> read n::Int) $ concatMap permutations $ combinations 3 "123456789"
f4 = map (\n -> read n::Int) $ concatMap permutations $ combinations 4 "123456789"


--prods = (*) <$> ff <*> sf
tuples = [((show f) ++ (show s) ++ (show $ f * s),((f, s), f*s)) | f <- (f1++f2++f3), s <- (f2++f3++f4), f < s]
panres = filter (\p -> isPanDig $ fst p) tuples
res = sum $ nub $ map (snd.snd) panres








