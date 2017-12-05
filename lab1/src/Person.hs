module Person where


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
unpack_person [SqlInteger id, SqlByteString first_name, SqlByteString last_name, SqlByteString position] =
  Person id (BS.unpack first_name) (BS.unpack last_name) (BS.unpack position)
unpack_person x = error $ "Unexpected result: " ++ show x

read_all_persons :: Connection -> IO [Person]
read_all_persons conn = quickQuery conn query [] >>= handle_result where
  query = "SELECT * FROM person ORDER BY id"
  handle_result result = return $ map unpack_person result

read_person :: Id -> Connection -> IO Person
read_person id conn = quickQuery conn query params >>= handle_result where
  query = "SELECT * FROM person WHERE id = ?"
  params = [SqlInteger id]
  handle_result result = return $ head $ map unpack_person result



update_person :: Id -> FirstName -> LastName -> Position -> Connection -> IO Bool
update_person id first_name last_name position connection = 
  run connection query params >>= (\changed -> return $ changed == 1) where
    query = "UPDATE person SET first_name = ?, last_name = ?, position = ? WHERE id = ?"
    params = map (SqlByteString . BS.pack) [first_name, last_name, position] ++ [SqlInteger id]

delete_person :: Id -> Connection -> IO Bool
delete_person id connection = 
  run connection query params >>= (\changed -> return $ changed == 1) where
    query = "DELETE FROM person WHERE id = ?"
    params = [SqlInteger id]

create_person :: FirstName -> LastName -> Position -> Connection -> IO Bool
create_person first_name last_name position connection =
  run connection query params >>= (\changed -> return $ changed == 1) where 
    query = "INSERT INTO person (first_name, last_name, position) values(?, ?, ?)"
    params = map (SqlByteString . BS.pack) [first_name, last_name, position]