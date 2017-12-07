module Lib where 

import Data.Time
import Data.Time.LocalTime


is_success_db_operation :: Integer -> IO Bool
is_success_db_operation = return . (== 1)


parse_time :: String -> TimeOfDay
parse_time dateStr = parseTimeOrError True defaultTimeLocale "%H:%M:%S" dateStr :: TimeOfDay