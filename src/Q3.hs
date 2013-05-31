module Q3
  where

bignum = 600851475143

minus (x:xs) (y:ys) = 
    case (compare x y) of
        LT -> x : minus xs (y:ys)
        EQ -> minus xs ys
        GT -> minus (x:xs) ys
minus xs _ = xs

union (x:xs) (y:ys) = 
    case (compare x y) of
        LT -> x : minus xs (y:ys)
        EQ -> x : minus xs ys
        GT -> y : minus (x:xs) ys


primesTo :: Integer -> [Integer]
primesTo n = 2 : sieve [2..n]
  where
    sieve [] = []
    sieve (p:xs) = p : sieve (xs `minus` [p*p, p*p+2*p..n])

answer = let ps = reverse . primesTo . floor . sqrt . fromIntegral $ bignum
         in head . filter (\x -> bignum `rem` x == 0) $ ps
