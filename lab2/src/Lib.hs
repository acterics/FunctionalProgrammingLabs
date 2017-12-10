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


pollard_rho :: Integer -> Integer -> Integer -> IO Integer
pollard_rho a b p = pollard_step_base a b p 0 0 0 0 1 1 0

-- new_step_value :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> [Integer]
-- new_step_value a b p x ai bi  
--     | (mod x 3) == 0 = [mod (x^2) p, mod (ai * 2) (p - 1), mod (bi * 2) (p - 1)]  
--     | (mod x 3) == 1 = [mod (x * a) p, mod (ai + 1) (p - 1), mod bi (p - 1)]
--     | (mod x 3) == 2 = [mod (x * b) p, mod ai (p - 1), mod(bi + 1) (p - 1)]
f :: Integer -> Integer -> Integer -> Integer -> Integer
f a b p x 
    | (mod x 3) == 0 = mod (x^2) p
    | (mod x 3) == 1 = mod (x * a) p
    | (mod x 3) == 2 = mod (x * b) p

g :: Integer -> Integer -> Integer -> Integer
g p x n
    | (mod x 3) == 0 = mod (2 * n) (p - 1) 
    | (mod x 3) == 1 = mod (n + 1) (p - 1)
    | (mod x 3) == 2 = mod n (p - 1)

h :: Integer -> Integer -> Integer -> Integer
h p x n
    | (mod x 3) == 0 = mod (2 * n) (p - 1) 
    | (mod x 3) == 1 = mod n (p - 1)
    | (mod x 3) == 2 = mod (n + 1) (p - 1) 

-- g :: Integer -> Integer -> Integer -> Integer
-- g p x n
--     | (mod x 3) == 0 = mod (2 * n) p 
--     | (mod x 3) == 1 = mod (n + 1) p
--     | (mod x 3) == 2 = mod n p

-- h :: Integer -> Integer -> Integer -> Integer
-- h p x n
--     | (mod x 3) == 0 = mod (2 * n) p 
--     | (mod x 3) == 1 = mod n p
--     | (mod x 3) == 2 = mod (n + 1) p 

pollard_step_base :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> Integer -> IO Integer
pollard_step_base a b p ai a2i bi b2i xi x2i i = do
    log
    if xi == x2i && i /= 0 then if bdif /= 0 then return $ mod (quot adif bdif) (p - 1)  else return (-1)
    else new_step (i + 1) ival i2val where
        adif = a2i - ai
        bdif = bi - b2i
        new_step i' [xi', ai', bi'] [x2i', a2i', b2i'] = pollard_step_base a b p ai' a2i' bi' b2i' xi' x2i' i'
        ival = [f a b p xi, g p xi ai, h p xi bi]
        i2val = [f a b p get_x2i, g p get_x2i get_a2i, h p get_x2i get_b2i]
        get_x2i = f a b p x2i
        get_a2i = g p x2i a2i
        get_b2i = h p x2i b2i
        log = putStrLn $ foldl (++) "" [
            show a, "^x = ", show b, " % ", show p,
            " step: ", show i,
            " ai:", show ai,
            " bi:", show bi,
            " xi:", show xi,
            " a2i:", show a2i,
            " b2i:", show b2i,
            " x2i:", show x2i
            ] 

    