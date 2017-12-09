-- Process 'update' commands
module UpdateCommand(process) where

import Lib
import Database.HDBC.PostgreSQL (Connection)
import Data.List
import Person
import Section

process :: Connection -> [String] -> IO Integer
process connection ("person":_) = update_person_fields connection
process connection ("section":_) = update_section_fields connection
process _ (_:_) = return continue_code

update_person_fields :: Connection -> IO Integer
update_person_fields connection = 
    get_fields ["person id", "new first name", "new last name", "new position"] >>= update_person_fields' connection
            

update_person_fields' :: Connection -> [String] -> IO Integer 
update_person_fields' connection [id, first_name, last_name, position] =
    update_person (read id) first_name last_name position connection >>= check_result
        
update_person_fields' _ x = do
    putStrLn $ "Invalid fields " ++ show x
    return continue_code


update_section_fields :: Connection -> IO Integer
update_section_fields connection = 
    get_fields ["section id", "new title"] >>= update_section_fields' connection

update_section_fields' :: Connection -> [String] -> IO Integer
update_section_fields' connection [id, title] = update_section (read id) title connection >>= check_result


get_empty_fields :: IO [String]
get_empty_fields = return []

get_field :: IO [String] -> String -> IO [String]
get_field x "" = x
get_field io_fields new_field = do
    putStrLn $ "Enter " ++ new_field
    getLine >>= \line -> io_fields >>= \fields -> return $ line : fields

get_fields :: [String] -> IO [String]
get_fields fields = foldl' get_field get_empty_fields $ reverse fields where

