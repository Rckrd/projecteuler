import Control.Applicative
import Data.List
import Data.Numbers.Primes

cg o s = o + 2*s^2

cgs = cg <$> (take 100 primes) <*> [1..100]
res = minimum $ [3,5..(maximum cgs)] \\ cgs

main :: IO ()
main = print res

-- 3, 5, 7
-- 
{--
gennodd :: Int
gennodd = gennodd' 3 
  where gennodd' n 
          | take 1
          | otherwise = [4]
        nextBiggerPrime = take 1 $ dropWhile (<n) primes
--}

