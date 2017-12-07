module Console where

import Lib
import Prelude
import Database.HDBC.PostgreSQL (Connection)
import Data.List.Split
import ShowCommand
import UpdateCommand

start_cli :: Connection -> IO ()
start_cli connection = do
    putStrLn "Welcome to Lab1 Application"
    process_commands connection continue_code
    putStrLn "#######################################################"


process_command :: Connection -> String -> IO Integer
process_command _ "exit" = return exit_success_code

process_command _ "help" = do
    putStrLn "help - Show commands description"
    putStrLn "show - Show table content"
    putStrLn "exit - Exit from application"
    return continue_code

process_command connection command = process_command_with_args connection $ parse_command command

process_command_with_args :: Connection -> [String] -> IO Integer
process_command_with_args connection (command:arguments) = 
    case command of
        "show" -> ShowCommand.process connection arguments 
        "update" -> UpdateCommand.process arguments
        _ -> return exit_fail_code
   
parse_command :: String -> [String]
parse_command = splitOn " "


process_commands :: Connection -> Integer -> IO Integer
    
process_commands connection 2 = do
    getLine >>= process_command connection  >>= process_commands connection

process_commands _ 0 = do
    putStrLn "Exit without errors"
    return 0

process_commands _ 1 = do
    putStrLn "Exit with error"
    return 1

