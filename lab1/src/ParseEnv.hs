module ParseEnv where 

import Data.List.Split


file_value_list:: String -> [String]
file_value_list file = splitOneOf  "=\n" file

concat_connection :: String -> [Char] -> Integer -> String
concat_connection conn s 0 = conn++" password="++s++" "
concat_connection conn s 1 = conn++" user="++s++" "
concat_connection conn s 2 = conn++" dbname="++s

concat_connection_base :: String -> (Integer, [Char]) -> String
concat_connection_base conn c = concat_connection conn (snd c) (fst c)


get_connection_string :: [(Integer, [Char])] -> String
get_connection_string parsed_env = foldl concat_connection_base "host=localhost" parsed_env


get_env_db_connection :: String -> String
get_env_db_connection = get_connection_string . zip [0..] . map snd . filter get_even_index . zip [0..] . file_value_list where
    get_even_index = odd . fst 