-- Process 'delete' command
module DeleteCommand(process) where

import Lib
import Database.HDBC.PostgreSQL (Connection)
import Person
import Section

process :: Connection -> [String] -> IO Integer
process _ ("help":_) = process_help

process connection ("person":args) = process_person connection args
process connection ("persons":args) = process_persons connection args

process connection ("section":args) = process_section connection args
process connection ("sections":args) = process_sections connection args

process connection ("schedule":args) = return continue_code

process _ args = show_error "delete" args

process_help :: IO Integer
process_help = mapM_ putStrLn help_text >> return continue_code where
    help_text = [
        "person <id> - delete person with id = <id>",
        "persons <first_name> <last_name> - delete persons with specified first name and last name",
        "persons <position> - delete persons with specified position",
        "section <id> - delete section with id = <id>",
        "sections <title> - delete sections with specified title"
        ]


process_persons :: Connection -> [String] -> IO Integer
process_persons connection [first_name, last_name] = delete_persons_by_name first_name last_name connection >>= check_result
process_persons connection [person_position] = delete_persons_by_position position connection >>= check_result
process_persons _ args = show_error "delete persons" args

process_person :: Connection -> [String] -> IO Integer
process_person connection [person_id] = delete_person person_id connection >>= check_result
process_person _ args = show_error "delete person" args

process_section :: Connection -> [String] -> IO Integer
process_section connection [section_id] = return continue_code
process_section _ args = show_error "delete section" args

process_sections :: Connection -> [String] -> IO Integer
process_sections connection [section_title] = return continue_code
process_sections _ args = show_error "delete sections" args

