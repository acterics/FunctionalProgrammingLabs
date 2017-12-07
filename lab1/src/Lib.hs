module Lib where 

is_success_db_operation :: Integer -> IO Bool
is_success_db_operation = return . (== 1)