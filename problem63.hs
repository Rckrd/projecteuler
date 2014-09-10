kup = map (length.show) $ map (^n) [1..15]
blag k = filter (==k) (kup k)
res = length $ concatMap blag [1..21]
