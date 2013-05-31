module Q2
  where 

fibs = 1:1:zipWith (+) fibs (tail fibs)

answer :: Integer
answer = sum . filter even . takeWhile (<=4000000) $ fibs
