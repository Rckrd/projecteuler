-- {a,b,c} such that p = 120 (omkrets) -> {20,48,52}, {24,45,51}
-- for which p <= 1000, is the number of solutions maximised

triangles :: Int -> [(Int, Int, Int)]
triangles p = [(a,b,c) | a <- [1..p], b <- [1..p-a], c <- [p -a -b], a <= b, b <= c, a^2+b^2 == c^2, a + b + c == p]

countTriangles :: Int -> (Int, Int)
countTriangles p = (p, length $ triangles p)
