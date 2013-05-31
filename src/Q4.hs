module Q4
  where

import Data.List
import Control.Monad
-- decimal nums are of the form
-- a + 10 * b + 100 * c

num2str :: Int -> String  
num2str = show 
                  
palindromes = 
    do x <- [100..999] 
       y <- [100..(x-1)]
       let p = x*y
       let pstr = show p
       guard $ pstr == reverse pstr
       return p

answer = maximum palindromes
