module Lib(
    pollard_rho
) where

-- func :: Integer -> Integer -> Integer
-- func x n = mod ( x * x - 1) n

-- pollardStep :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
-- pollardStep i k n x y
--         | d /= 1 && d /= n = d
--         | i == k = pollardStep (i+1) (2*k) n x1 x1
--         | otherwise = pollardStep (i+1) k n x1 y where
--              d = gcd n $ abs $ y - x
--              x1 = func x n

-- pollard_rho :: Integer -> Integer
-- pollard_rho n = pollardStep 1 2 n 2 2



pollard_rho a b p = pollard_step_base a b p 0 0 0 0 1 1 1

new_step_value :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
new_step_value a b p x ai bi  
    | (mod x 3) == 0 = map f [x^2, ai * 2, bi * 2]  
    | (mod x 3) == 1 = map f [x * a, ai + 1, bi]
    | (mod x 3) == 2 = map f [x * b, ai, bi + 1] where
        f x' = mod x' p
    

pollard_step_base :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer
pollard_step_base a b p ai a2i bi b2i xi x2i i = 
    if xi == x2i then 
        if bdif /= 0 then mod (quot adif bdif) p  
        else -1
    else new_step (i + 1) ival i2val where
        adif = a2i - ai
        bdif = bi - b2i
        new_step i' [xi', ai', bi'] [x2i', a2i', b2i'] = pollard_step_base a b p ai' a2i' bi' b2i' xi' x2i' i'
        ival = new_step_value a b p xi ai bi
        i2val = new_step_value a b p x2i a2i b2i

    