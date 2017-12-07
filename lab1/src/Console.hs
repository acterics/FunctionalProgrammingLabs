module Console where


import Person
import Section
import Prelude

column_length :: Int
column_length = 15

show_person :: Person -> IO ()
show_person person = 
    putStrLn $ (get_field_with_spaces $ show $ Person.id person) ++ (get_field_with_spaces $ firstName person) ++ 
               (get_field_with_spaces $ lastName person) ++ (get_field_with_spaces $ position person)

show_section :: Section -> IO ()
show_section section = 
    putStrLn $ (get_field_with_spaces $ show $ Section.id section) ++ (get_field_with_spaces $ Section.title section)

show_persons_list :: [Person] -> IO ()
show_persons_list persons = do
    putStrLn get_person_table_headers
    mapM_ show_person persons

show_sections_list :: [Section] -> IO ()
show_sections_list sections = do
    putStrLn get_section_table_headers
    mapM_ show_section sections





get_spaces :: String -> String
get_spaces input = foldl add "" [0..get_spaces_count input] where
    add spaces _ = spaces ++ " "
    get_spaces_count = (-) column_length . length

get_field_with_spaces :: String -> String
get_field_with_spaces input = input ++ get_spaces input

get_person_table_headers :: String
get_person_table_headers = (get_field_with_spaces "ID") ++ (get_field_with_spaces "FIRST_NAME") ++ (get_field_with_spaces "LAST_NAME") ++ (get_field_with_spaces "POSITION")

get_section_table_headers :: String
get_section_table_headers = (get_field_with_spaces "ID") ++ (get_field_with_spaces "TITLE")
