import Data.List
-- p :: Int -> Int
p n = fromInteger.round $ n * (3*n - 1)/2

--n^2 - n/3 - 2*p/3  = 0
n1 p = 1/6 + sqrt(1/36+2*p/3)
n2 p = 1/6 - sqrt(1/36+2*p/3)

--ipen :: (RealFrac a, Floating a) => a -> Bool
ipen = isInt.n1

isInt x = x == fromInteger (round x)

--triples' :: [(Int, (Int, Int))]
pens = map p [1..10000]
triples' = [((a-b), (a,b)) | a <- pens, b <- pens, a > b, ipen (a-b), ipen (a+b)] 
res = take 1 $ sortBy comptrip triples'
    where comptrip a b = a `compare` b



--------------
import 
main = print (take 1 solutions)
 where solutions = [a-b | a <- penta, b <- takeWhile (<a) penta,
                          isPenta (a-b), isPenta (b+a)]
       isPenta = (`Set.member` Set.fromList  penta)
       penta = [(n * (3*n-1)) `div` 2 | n <- [1..5000]]
------------------------
let {p n = n*(3*n-1)`div`2; isP n=(\x->x^2==24*n+1&&x`mod`6==5)$floor(sqrt(24*(fromIntegral n)+1))} in head [p1 - p2|n1<-[1..], n2<-[1..n1-1], p1<-[p n1],p2<-[p n2],isP$p1-p2,isP$p1+p2]


-------------------
pntg = map (\n -> (3 * n^2 - n) `div` 2) [1..]

isPntg :: Integral a => a -> Bool
isPntg x = (t x) `mod` 6 == 5 && checkT x
    where
        t x = floor $ sqrt $ fromIntegral (24 * x + 1)
        checkT x = (t x)^2 == 24 * x + 1

euler44 = head [p1-p2 | p1 <- pntg, p2 <- takeWhile(< p1) pntg, isPntg (p1-p2), isPntg (p1+p2)]

