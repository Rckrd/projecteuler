import Data.Numbers.Primes
import Data.List

main :: IO ()
main = putStr "hej"

res = [p | p <- take 100000 primes, all isPrime (numPieces p), length (show p) > 1] 

numPieces n = map (\n -> read n :: Int ) $ nub $ filter (\n -> length n > 0) $ (inits.show $ n) ++ (tails.show $ n)


