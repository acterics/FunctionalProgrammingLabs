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
    

-- Get database connection with specified in .env file database credentials
get_db_connection :: IO Connection
get_db_connection = readFile ".env" >>= connectPostgreSQL . get_env_connection_args

-- Util func for time
parse_time :: String -> TimeOfDay
parse_time dateStr = parseTimeOrError True defaultTimeLocale "%H:%M:%S" dateStr :: TimeOfDay

show_day :: Integer -> String
show_day 0 = "Mon"
show_day 1 = "Tue"
show_day 2 = "Wed"
show_day 3 = "Thu"
show_day 4 = "Fri"
show_day 5 = "Sat"
show_day 6 = "Sun"
show_day day_code = error $ "Illegal day code: " ++ show day_code

parse_day :: String -> Integer
parse_day "Mon" = 0
parse_day "Tue" = 1
parse_day "Wed" = 2
parse_day "Thu" = 3
parse_day "Fri" = 4
parse_day "Sat" = 5
parse_day "Sun" = 6
parse_day day = error $ "Illegal day: " ++ day