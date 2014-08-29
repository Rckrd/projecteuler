import Data.List

pandigits = filter (\s -> head s /= '0') $ permutations "0123456789"

f :: Int -> Int -> Int -> Int -> String -> Bool
f i j k l xs = mod (read [xs!!(i-1), xs!!(j-1), xs!!(k-1)]::Int) l == 0

res = sum $ [read x::Int | x <- pandigits, 
             f 2 3 4 2 x,
             f 3 4 5 3 x,
             f 4 5 6 5 x,
             f 5 6 7 7 x,
             f 6 7 8 11 x,
             f 7 8 9 13 x,
             f 8 9 10 17 x]
------------------------------ 
l n = [[x,y,z] | x <- ['0'..'9'], 
                 y <- ['0'..'9'], 
                 z <- ['0'..'9'], 
                 mod (read [x,y,z]) n == 0, 
                 x /= y, 
                 y /=x, 
                 x /= z]

fits l1 l2 = [ x:y | (x:xs) <- l1,
                      y <- l2, 
                      read xs - (read . take 2) y == 0,
                      not (elem x y)]

ans = sum $ map read $ foldr fits (l 17) (map l [1,2,3,5,7,11,13])

---------------
hasTheProperty nStr = all (\(num, div) -> num `mod` div == 0) $ zip nums divisors
  where nums = map toNum [take 3 $ iterate (+1) i | i <- [1..7]]
        toNum = read . map (nStr !!)
        divisors = [2, 3, 5, 7, 11, 13, 17]

answer = sum $ map read $ filter hasTheProperty $ permutations "1234567890"

