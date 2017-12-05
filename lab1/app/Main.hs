module Main where

import Person
import ParseEnv


main :: IO ()
main = do
    string <- readFile ".env"
    putStrLn (get_env_db_connection string)
    -- conn <- connectPostgreSQL (filter_list  string)

    -- conn <- connectPostgreSQL "host=localhost dbname=sport_univ_db user=postgres password=root"

    
