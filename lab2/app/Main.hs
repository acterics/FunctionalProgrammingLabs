module Main where

import Lib

main :: IO ()
main = pollard_rho 2 5 1019 >>= putStrLn . show

