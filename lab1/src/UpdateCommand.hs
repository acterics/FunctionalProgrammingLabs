-- Process 'update' commands
module UpdateCommand(process) where

import Lib
import Database.HDBC.PostgreSQL (Connection)
import Data.List
import Person
import Section

process :: Connection -> [String] -> IO Integer
process connection ("person":args) = process_person connection args
process connection ("section":args) = process_section connection args

process _ ("help":_) = process_help
process _ args = show_error "update" args


process_help :: IO Integer
process_help  = mapM_ putStrLn help_content >> return continue_code where
    help_content = [
        "person -a - step by step person update",
        "person <person_id> <new first name> <new last name> <new position> - update specified person",
        "section -a - step by step person update",
        "section <section_id> <section_title> - update specified section"
        ]

process_person :: Connection -> [String] -> IO Integer
process_person connection ("-a":_) = update_person_fields connection
process_person connection args = update_person_fields' connection args

process_section :: Connection -> [String] -> IO Integer
process_section connection ("-a":_) = update_section_fields connection
process_section connection args = update_section_fields' connection args


update_person_fields :: Connection -> IO Integer
update_person_fields connection = 
    get_fields ["person id", "new first name", "new last name", "new position"] >>= update_person_fields' connection
            

update_person_fields' :: Connection -> [String] -> IO Integer 
update_person_fields' connection [id, first_name, last_name, position] =
    update_person (read id) first_name last_name position connection >>= check_result
        
update_person_fields' _ args = show_error "update person" args


update_section_fields :: Connection -> IO Integer
update_section_fields connection = 
    get_fields ["section id", "new title"] >>= update_section_fields' connection

update_section_fields' :: Connection -> [String] -> IO Integer
update_section_fields' connection [id, title] = update_section (read id) title connection >>= check_result
update_section_fields' _ args = show_error "update section" args


get_empty_fields :: IO [String]
get_empty_fields = return []

get_field :: IO [String] -> String -> IO [String]
get_field x "" = x
get_field io_fields new_field = do
    putStrLn $ "Enter " ++ new_field
    getLine >>= \line -> io_fields >>= \fields -> return $ line : fields

get_fields :: [String] -> IO [String]
get_fields fields = foldl' get_field get_empty_fields $ reverse fields where

