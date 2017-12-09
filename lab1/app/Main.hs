module Main where

import Lib
import Console

main :: IO ()
main = get_db_connection >>= start_console



    
