

coinsums = [(p1, p2, p5, p10, p20, p50, p100, p200) |
    p200 <- [0..1],
    p100 <- [0..2    -2*p200],
     p50 <- [0..4    -4*p200 - 2*p100],
     p20 <- [0..10  -10*p200 - 5*p100 - (floor $ 2.5* fromInteger p50)],
     p10 <- [0..20  -20*p200 - 10*p100 - 5*p50 - 2*p20],
      p5 <- [0..40  -40*p200 - 20*p100 - 10*p50 - 4*p20 - 2*p10],
      p2 <- [0..100-100*p200 - 50*p100 - 25*p50 - 10*p20 - 5*p10 - (floor $ 2.5 * fromInteger p5)],
      p1 <- [0..200-200*p200 - 100*p100 - 50*p50 - 20*p20 - 10*p10 - 5*p5 - 2*p2],
      p1 + 2*p2 + 5*p5 + 10*p10 + 20*p20 + 50*p50 + 100*p100 + 200*p200 == 200
     ]



denom :: Int -> [Int] -> Int
denom n [] = 0

denom n l@(x:xs) 
  | n == 0 = 1
  | n < 0 = 0
  | otherwise = denom n xs + denom (n - x) l

pieces :: [Int]
pieces = [1,2,5,10,20,50,100,200]

beautiful = foldl (\without p ->
                          let (poor,rich) = splitAt p without
                              with = poor ++ 
                                     zipWith (++) (map (map (p:)) with)
                                                  rich
                          in with
                     ) ([[]] : repeat [])

main = print . length $ beautiful pieces !! 200

