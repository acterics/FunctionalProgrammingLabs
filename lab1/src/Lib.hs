module Lib where 

import Data.Time

exit_success_code :: Integer
exit_success_code = 0

exit_fail_code :: Integer
exit_fail_code = 1

continue_code :: Integer
continue_code = 2


is_success_db_operation :: Integer -> IO Bool
is_success_db_operation = return . (== 1)


parse_time :: String -> TimeOfDay
parse_time dateStr = parseTimeOrError True defaultTimeLocale "%H:%M:%S" dateStr :: TimeOfDay