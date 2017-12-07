module Main where

import Person
import Section
import ParseEnv
import Console
import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)


open_connection :: String -> IO Connection
open_connection file = connectPostgreSQL $ get_env_db_connection file

main :: IO ()
-- main =  readFile ".env" >>= open_connection >>= read_all_persons >>= (\persons -> putStrLn $ show persons)
main =  readFile ".env" >>= open_connection >>= compose_queries where 
    compose_queries connection = do
        read_all_persons connection >>= show_persons_list
        read_all_sections connection >>= show_sections_list
-- main =  readFile ".env" >>= open_connection >>= read_person 1 >>= (\person -> putStrLn $ show person)
-- main =  readFile ".env" >>= open_connection >>= show_persons



    
