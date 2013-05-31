module Q1
  where

f :: Int -> Int
f x = let xs = [1..(x - 1)]
          pred x = mod x 3 == 0 || mod x 5 == 0
      in sum $ [x | x <- xs, pred x]

answer = f 1000

