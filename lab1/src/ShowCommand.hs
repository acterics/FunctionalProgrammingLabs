module ShowCommand where

import Lib
import Database.HDBC.PostgreSQL (Connection)
import Person
import Section

process :: Connection -> [String] -> IO Integer

process _ [] = do
    putStrLn "Illegal no arguments for this command"
    return exit_fail_code

process connection (command:_) = 
    case command of
        "persons" -> read_all_persons connection >>= show_persons_list >> return continue_code
        "sections" -> read_all_sections connection >>= show_sections_list >> return continue_code
        "help" -> show_help
        _ -> return exit_fail_code

-- process arguments = do
--     putStrLn $ show arguments
--     return continue_code

column_length :: Int
column_length = 15

show_help :: IO Integer
show_help = do
    putStrLn "persons - Show person table"
    return continue_code

show_person :: Person -> IO ()
show_person person = putStrLn $ foldl (++) "" $ fields person where
    fields person = map get_field_with_spaces [show $ Person.id person , firstName person, lastName person, position person] 


show_persons_list :: [Person] -> IO ()
show_persons_list persons = do
    putStrLn get_person_table_headers
    mapM_ show_person persons


get_spaces :: String -> String
get_spaces input = foldl add "" [0..get_spaces_count input] where
    add spaces _ = spaces ++ " "
    get_spaces_count = (-) column_length . length

get_field_with_spaces :: String -> String
get_field_with_spaces input = input ++ get_spaces input

get_person_table_headers :: String
get_person_table_headers = foldl (++) "" fields where
    fields = map get_field_with_spaces ["ID", "FIRST_NAME", "LAST_NAME", "POSITION"]

get_section_table_headers :: String
get_section_table_headers = foldl (++) "" fields where
    fields = map get_field_with_spaces ["ID", "TITLE"]


show_section :: Section -> IO ()
show_section section = putStrLn $ foldl (++) "" $ fields section where
    fields section = map get_field_with_spaces [show $ Section.id section, Section.title section]



show_sections_list :: [Section] -> IO ()
show_sections_list sections = do
    putStrLn get_section_table_headers
    mapM_ show_section sections