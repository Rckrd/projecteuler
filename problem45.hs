import Data.List

tri n = round $ n * (n + 1) /2
pen n = round $ n*(3*n -1)/2
hex n = round $ n*(2*n-1)

trin = map tri [1..]
penn = map pen [1..]
hexn = map hex [1..]

res = [(x,y,z) | x<-[1..2000], y <- [1..1600], z <- [1..1000], tri x == pen y, pen y == hex z]

res3 = intersect (take 100000 trin) (take 100000 penn)

f (a:as) (b:bs) 
    | a < b = f as (b:bs)
    | a > b = f (a:as) bs
    | otherwise = a : f as bs
res2 = take 3 $ f (scanl1 (+) [1,4..]) (scanl1 (+) [1,5..])


isPent n = t == 0
    where (_, t) = properFraction $ (1/3) + sqrt((1/9) + (8/3)*(fromInteger n))

hexags = map (\x -> x*(2*x - 1)) [144..] -- start at 144th hexagonal number

pe45 = head (filter isPent hexags)

