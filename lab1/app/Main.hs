module Main where

import Person
import Data.List.Split
import Data.List
import Data.Ord


file_value_list:: String -> [String]
file_value_list file = splitOneOf  "=\n" file

filter_list :: String -> String
filter_list = show . map snd . filter get_even_index . zip [0..] . file_value_list where
    get_even_index = odd . fst 
-- filter_even = map (snd) filter_list where
--     filter_list = filter (\(i, _) -> even i) . zip [0..]
    



main :: IO ()
main = do
    string <- readFile ".env"
    
    putStrLn (filter_list string)

    -- conn <- connectPostgreSQL "host=localhost dbname=sport_univ_db user=postgres password=root"

    
