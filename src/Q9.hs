module Q9
  where

import Control.Monad

triples = 
    do x <- [1..1000]
       y <- [1..(x-1)]
       z <- [1..(y-1)]
       guard $ x + y + z == 1000
       guard $ x^2 ==  y^2 + z^2
       return (x,y,z)
answer = head triples
