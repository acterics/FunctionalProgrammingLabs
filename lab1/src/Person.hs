module Person where

import Lib
import Prelude hiding (read)
import Database.HDBC
import Database.HDBC.PostgreSQL
import qualified Data.ByteString.Char8 as BS


type Id = Integer
type FirstName = String
type LastName = String
-- data Position = Student | Teacher deriving (Enum, Show)
type Position = String

data Person = Person { id :: Id, firstName :: FirstName, lastName :: LastName, position :: Position } deriving (Show)



unpack_person :: [SqlValue] -> Person
unpack_person [SqlInteger person_id, SqlByteString first_name, SqlByteString last_name, SqlByteString position] =
  Person person_id (BS.unpack first_name) (BS.unpack last_name) (BS.unpack position)
unpack_person x = error $ "Unexpected result: " ++ show x

unpack_person_list :: [[SqlValue]] -> IO [Person]
unpack_person_list = return . map unpack_person

unpack_person_from_list :: [[SqlValue]] -> IO Person
unpack_person_from_list = return . head . map unpack_person

read_all_persons :: Connection -> IO [Person]
read_all_persons conn = quickQuery conn query [] >>= handle_result where
  query = "SELECT * FROM person ORDER BY id"
  handle_result = return . map unpack_person

read_person :: Id -> Connection -> IO Person
read_person person_id conn = quickQuery conn query params >>= unpack_person_from_list where
  query = "SELECT * FROM person WHERE id = ?"
  params = [SqlInteger person_id]


read_section_persons :: Id -> Connection -> IO [Person]
read_section_persons section_id connection = quickQuery connection query params >>= unpack_person_list where
  query = "SELECT * FROM person WHERE id IN " ++ 
          "(SELECT person_id FROM section_participant WHERE section_id = ?)"
  params = [SqlInteger section_id]



update_person :: Id -> FirstName -> LastName -> Position -> Connection -> IO Bool
update_person person_id first_name last_name pos connection = 
  run connection query params >>= is_success_db_operation where
    query = "UPDATE person SET first_name = ?, last_name = ?, position = ? WHERE id = ?"
    params = map (SqlByteString . BS.pack) [first_name, last_name, pos] ++ [SqlInteger person_id]

delete_person :: Id -> Connection -> IO Bool
delete_person person_id connection = 
  run connection query params >>= is_success_db_operation where
    query = "DELETE FROM person WHERE id = ?"
    params = [SqlInteger person_id]

create_person :: FirstName -> LastName -> Position -> Connection -> IO Bool
create_person first_name last_name pos connection =
  run connection query params >>= is_success_db_operation where 
    query = "INSERT INTO person (first_name, last_name, position) VALUES(?, ?, ?)"
    params = map (SqlByteString . BS.pack) [first_name, last_name, pos]