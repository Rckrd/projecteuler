
isPalin n = show n == (reverse.show) n

makePalin :: Integer -> Integer
makePalin n 
  | isPalin n = n
  | otherwise = makePalin (iterateL n)

iterateL n = n + read ((reverse.show) n)::Integer
