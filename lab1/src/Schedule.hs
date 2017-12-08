module Schedule where

import Lib

import Data.Time
import Data.Time.LocalTime
import Prelude hiding (read)
import Database.HDBC
import Database.HDBC.PostgreSQL
import qualified Data.ByteString.Char8 as BS

type Id = Integer
type ScheduleDay = Integer
type RawTime = String

data Schedule = Schedule{ id :: Id, section_id :: Id, day :: ScheduleDay, time_start :: TimeOfDay, time_end :: TimeOfDay }

unpack_schedule :: [SqlValue] -> Schedule
unpack_schedule [SqlInteger schedule_id, SqlInteger sec_id, SqlInteger day_of_week, SqlLocalTimeOfDay begin, SqlLocalTimeOfDay end] = 
    Schedule schedule_id sec_id day_of_week begin end
unpack_schedule x = error $ "Unexpected result: " ++ show x

unpack_schedule_list :: [[SqlValue]] -> IO [Schedule]
unpack_schedule_list = return . map unpack_schedule

read_all_schedule :: Connection -> IO [Schedule]
read_all_schedule connection = quickQuery connection query [] >>= unpack_schedule_list where 
    query = "SELECT * FROM schedule ORDER BY day"

read_day_schedule :: ScheduleDay -> Connection -> IO [Schedule]
read_day_schedule schedule_day connection = quickQuery connection query params >>= unpack_schedule_list where
    query = "SELECT * FROM schedule WHERE day = ? ORDER BY time_start"
    params = [SqlInteger schedule_day]

read_section_schedule :: Id -> Connection -> IO [Schedule]
read_section_schedule section_id connection = quickQuery connection query params >>= unpack_schedule_list where
    query = "SELECT * FROM schedule WHERE section_id = ?"
    params = [SqlInteger section_id]

update_schedule :: Id -> Id -> ScheduleDay -> RawTime -> RawTime -> Connection -> IO Bool
update_schedule schedule_id section_id schedule_day begin end connection = 
    run connection query params >>= is_success_db_operation where
        query = "UPDATE schedule SET section_id = ?, day_of_week = ?, time_start = ?, time_end = ? WHERE id = ?"
        params = [SqlInteger section_id, SqlInteger schedule_day, SqlLocalTimeOfDay $ parse_time begin, SqlLocalTimeOfDay $ parse_time end, SqlInteger schedule_id]

update_section_day_schedule :: Id -> ScheduleDay -> RawTime -> RawTime -> Connection -> IO Bool
update_section_day_schedule section_id schedule_day begin end connection = 
    run connection query params >>= is_success_db_operation where
        query = "UPDATE schedule SET time_start = ?, time_end = ? WHERE section_id = ? AND day_of_week = ?"
        params = [SqlLocalTimeOfDay $ parse_time begin, SqlLocalTimeOfDay $ parse_time end, SqlInteger section_id, SqlInteger schedule_day]

delete_schedule :: Id -> Connection -> IO Bool 
delete_schedule schedule_id connection = run connection query params >>= is_success_db_operation where
    query = "DELETE FROM schedule WHERE id = ?"
    params = [SqlInteger schedule_id]

delete_section_day_schedule :: Id -> ScheduleDay -> Connection -> IO Bool
delete_section_day_schedule section_id schedule_day connection =
    run connection query params >>= is_success_db_operation where
        query = "DELETE FROM schedule WHERE section_id = ? AND day_of_week = ?"
        params = [SqlInteger section_id, SqlInteger schedule_day]

create_schedule :: Id -> ScheduleDay -> RawTime -> RawTime -> Connection -> IO Bool
create_schedule section_id schedule_day begin end connection =
    run connection query params >>= is_success_db_operation where
        query = "INSERT INTO schedule (section_id, day_of_week, time_start, time_end) VALUES(?, ?, ?, ?)"
        params = [SqlInteger section_id, SqlInteger schedule_day, SqlLocalTimeOfDay $ parse_time begin, SqlLocalTimeOfDay $ parse_time end]



    