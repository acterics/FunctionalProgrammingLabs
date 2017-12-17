-- Process 'show' commands
module ShowCommand(process) where

import Lib
import Database.HDBC.PostgreSQL (Connection)
import Person
import Section
import Schedule

process :: Connection -> [String] -> IO Integer
process connection ("persons":_) = read_all_persons connection >>= show_persons_list >> return continue_code
process connection ("sections":_) = read_all_sections connection >>= show_sections_list >> return continue_code
process connection ("schedule":args) = process_schedule args connection
process _ ("help":_) = show_help
process _ args = show_error "show" args


process_schedule :: [String] -> Connection -> IO Integer
process_schedule ("week":_) connection = read_all_schedule connection >>= show_schedule >> return continue_code
process_schedule ("section":section_name:_) connection = read_section_schedule section_name connection >>= show_schedule >> return continue_code
process_schedule ("help":_) _ = show_schedule_help 
process_schedule args _ = show_error "show schedule" args

column_length :: Int
column_length = 15


show_help :: IO Integer
show_help = mapM_ putStrLn help_content >> return continue_code where
    help_content = [
        "persons - Show person table",
        "sections - Show sections table",
        "schedule - Show schedule"
        ]

show_schedule_help :: IO Integer
show_schedule_help = mapM_ putStrLn help_content >> return continue_code where
    help_content = [
        "week - show week schedule",
        "section <section_name> - show schedule for section"
        ]

show_person :: Person -> IO ()
show_person person = putStrLn $ foldl (++) "" fields where
    fields = map get_field_with_spaces [
        show $ Person.id person, 
        firstName person, 
        lastName person, 
        position person
        ] 

show_persons_list :: [Person] -> IO ()
show_persons_list persons = putStrLn get_person_table_headers >> mapM_ show_person persons
    
get_person_table_headers :: String
get_person_table_headers = foldl (++) "" fields where
    fields = map get_field_with_spaces ["ID", "FIRST_NAME", "LAST_NAME", "POSITION"]

show_section :: Section -> IO ()
show_section section = putStrLn $ foldl (++) "" $ fields where
    fields = map get_field_with_spaces [show $ Section.id section, Section.title section]


show_sections_list :: [Section] -> IO ()
show_sections_list sections = do
    putStrLn get_section_table_headers
    mapM_ show_section sections

get_section_table_headers :: String
get_section_table_headers = foldl (++) "" fields where
    fields = map get_field_with_spaces ["ID", "TITLE"]

show_schedule_row :: Schedule -> IO ()
show_schedule_row schedule_row = putStrLn $ foldl (++) "" fields where
    fields = map get_field_with_spaces [
        show_day $ day schedule_row,
        section_title schedule_row,
        show $ time_start schedule_row,
        show $ time_end schedule_row
        ]

show_schedule :: [Schedule] -> IO()
show_schedule schedule= putStrLn get_schedule_table_headers >> mapM_ show_schedule_row schedule 

get_schedule_table_headers :: String
get_schedule_table_headers = foldl (++) "" fields where
    fields = map get_field_with_spaces ["DAY", "SECTION", "FROM", "TO"]
    
    
get_spaces :: String -> String
get_spaces input = foldl add "" [0..get_spaces_count input] where
    add spaces _ = spaces ++ " "
    get_spaces_count = (-) column_length . length

get_field_with_spaces :: String -> String
get_field_with_spaces input = input ++ get_spaces input