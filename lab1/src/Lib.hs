module Lib where 

import Data.Time
import ParseEnv
import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)

-- Result codes

exit_success_code :: Integer
exit_success_code = 0

exit_fail_code :: Integer
exit_fail_code = 1

continue_code :: Integer
continue_code = 2



is_success_db_operation :: Integer -> IO Bool
is_success_db_operation = return . (== 1)


check_result :: Bool -> IO Integer
check_result result = if result then return continue_code else return exit_fail_code

show_error :: String -> [String] -> IO Integer
show_error command args = mapM_ putStrLn error_info >> return continue_code where
    error_info = [
        "Illegal args: " ++ (show args) ++ " for command " ++ command,
        "Use '" ++ command ++ " help' for information about available arguments"
        ]
    

-- Util func for time
parse_time :: String -> TimeOfDay
parse_time dateStr = parseTimeOrError True defaultTimeLocale "%H:%M:%S" dateStr :: TimeOfDay

get_db_connection :: IO Connection
get_db_connection = readFile ".env" >>= connectPostgreSQL . get_env_connection_args

show_day :: Integer -> String
show_day 0 = "Mon"
show_day 1 = "Tue"
show_day 2 = "Wed"
show_day 3 = "Thu"
show_day 4 = "Fri"
show_day 5 = "Sat"
show_day 6 = "Sun"

parse_day :: String -> Integer
parse_day "Mon" = 0
parse_day "Tue" = 0
parse_day "Wed" = 0
parse_day "Thu" = 0
parse_day "Fri" = 0
parse_day "Sat" = 0
parse_day "Sun" = 0