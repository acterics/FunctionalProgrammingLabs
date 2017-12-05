module Main where

import Person
import ParseEnv
import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)


open_connection :: String -> IO Connection
open_connection file = connectPostgreSQL $ get_env_db_connection file

main :: IO ()
-- main =  readFile ".env" >>= open_connection >>= read_all_persons >>= (\persons -> putStrLn $ show persons)
main =  readFile ".env" >>= open_connection >>= read_person 1 >>= (\person -> putStrLn $ show person)



    
