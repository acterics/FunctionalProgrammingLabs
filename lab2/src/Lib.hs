module Lib(
    pollard_rho
) where

--Solve a^x = b (mod p) task
pollard_rho :: Integer -> Integer -> Integer -> IO Integer
pollard_rho a b p = pollard_step_base [a, b, p, 0, 0, 0, 0, 1, 1, 0]

pollard_step_base :: [Integer] -> IO Integer
pollard_step_base [a, b, p, ai, a2i, bi, b2i, xi, x2i, i] = log_step >>
    if xi == x2i && i /= 0
    then if bdif /= 0
        then if gcd' == 1
            then log_result >> (return $ mod (quot adif bdif) (p - 1))
            else log_result >> log_exception >> enum_values 0
        else error "Factor difference can't be zero"
    else new_step (i + 1) (new_values a b p [xi, ai, bi]) (new_values a b p $ new_values a b p [x2i, a2i, b2i]) 
    where
        adif = abs (ai - a2i)
        bdif = abs (b2i - bi)
        new_step i' [xi', ai', bi'] [x2i', a2i', b2i'] = pollard_step_base [a, b, p, ai', a2i', bi', b2i', xi', x2i', i']
        enum_values m = log_enum m >> if m <= (gcd' - 1)
                        then if (mod (a ^ (get_enum_x m)) p == b)
                             then return $ (get_enum_x m)
                             else enum_values (m + 1)                       
                        else error "There's no result"
        gcd' = gcd (p - 1) bdif
        get_x0 = mod (quot adif bdif) (quot (p - 1) gcd')
        get_enum_x m = get_x0 + (quot (m * (p - 1)) gcd')
        -- Functions for logging
        log_result = putStrLn $ foldl (++) "" [
            show a, "^", show ai, "*", show b, "^", show bi, 
            " = ", show xi, " = ",
            show a, "^", show a2i, "*", show b, "^", show b2i, "\n",
            "(", show b2i, "-", show bi, ")x0 = (", show ai, "-", show a2i, ")\n", 
            "x0 = ", show adif, "/", show bdif
            ]
        log_step = putStrLn $ foldl (++) "" [
            show a, "^x = ", show b, " % ", show p,
            " step: ", show i,
            " ai:", show ai,
            " bi:", show bi,
            " xi:", show xi,
            " a2i:", show a2i,
            " b2i:", show b2i,
            " x2i:", show x2i
            ] 
        log_exception = putStrLn $ foldl (++) "" [
             "d = GCD(", show bdif, ", ", show (p - 1), ") = ", show gcd', "\n",
             "x0 = ", show get_x0 
            ]
        log_enum m = putStrLn $ foldl (++) "" [
            "x = ", show get_x0, " + ", show m, " * ", show (p - 1), "/", show gcd', " = ",  show $ get_enum_x m
            ]
pollard_step_base x = error $ "Illegal arguments: " ++ show x

new_values :: Integer -> Integer -> Integer -> [Integer] -> [Integer]
new_values a b p [x', a', b'] 
    | (mod x' 3) == 0 = [mod (x'^2) p, mod (a' * 2) (p - 1), mod (b' * 2) (p - 1)]  
    | (mod x' 3) == 1 = [mod (x' * a) p, mod (a' + 1) (p - 1), b']
    | (mod x' 3) == 2 = [mod (x' * b) p, a', mod(b' + 1) (p - 1)]