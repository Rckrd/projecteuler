import Data.Char

digsum n = sum $ map digitToInt $ show n
res = maximum [digsum (a^b) | a <- [1..100], b <- [1..100]]
