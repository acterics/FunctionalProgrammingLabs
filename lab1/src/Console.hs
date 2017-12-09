module Console(start_console) where

import Lib
import Prelude
import Database.HDBC.PostgreSQL (Connection)
import Data.List.Split
import ShowCommand
import UpdateCommand


-- Entry point for application
start_console :: Connection -> IO ()
start_console connection = do
    putStrLn "Welcome to Lab1 Application"
    process_commands connection continue_code
    putStrLn ""


-- Recursive processing of user input
process_commands :: Connection -> Integer -> IO Integer
process_commands connection 2 = putStrLn "" >> getLine >>= process_command connection  >>= process_commands connection
process_commands _ 0 = putStrLn "Exit without errors" >> return 0
process_commands _ 1 = putStrLn "Exit with error" >> return 1
    
-- Process command and delegating processing to other command modules
process_command :: Connection -> String -> IO Integer
process_command _ "exit" = return exit_success_code
process_command _ "help" = mapM_ putStrLn help_text >> return continue_code where
        help_text = [
            "help - Show commands description",
            "show - Show table content",
            "update - Update table content",
            "exit - Exit from application"
            ] 
process_command connection command  = process_command_with_args connection $ parse_command command



process_command_with_args :: Connection -> [String] -> IO Integer
process_command_with_args connection (command:arguments) = process_command_with_args' command connection arguments

process_command_with_args' :: String -> Connection -> [String] -> IO Integer
process_command_with_args' "show" = ShowCommand.process
process_command_with_args' "update" = UpdateCommand.process
process_command_with_args' command = \ _ _ -> show_error "" [command]

   
parse_command :: String -> [String]
parse_command = splitOn " "




