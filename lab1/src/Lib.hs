module Lib where 

import Data.Time
import ParseEnv
import Database.HDBC.PostgreSQL (connectPostgreSQL, Connection)

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


parse_time :: String -> TimeOfDay
parse_time dateStr = parseTimeOrError True defaultTimeLocale "%H:%M:%S" dateStr :: TimeOfDay

get_db_connection :: IO Connection
get_db_connection = readFile ".env" >>= connectPostgreSQL . get_env_connection_args