import Data.Char

--main :: ()
main = putStr $ show $ product $ map (digitToInt . (\n -> nums !! n)) tens 
    where tens = [1,10,100,1000,1000,10000,100000]
          nums = concatMap (show) [0..]
