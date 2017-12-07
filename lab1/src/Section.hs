module Section where

import Lib
import Prelude hiding (read)
import Database.HDBC
import Database.HDBC.PostgreSQL
import qualified Data.ByteString.Char8 as BS

type SectionTitle = String
type Id = Integer

data Section = Section {id :: Id, title :: SectionTitle} deriving (Show)
-- data SectionParticipation = SectionParticipation {section_id :: Integer, person_id :: Integer }

unpack_section :: [SqlValue] -> Section
unpack_section [SqlInteger section_id, SqlByteString section_title] =
  Section section_id (BS.unpack section_title)
unpack_section x = error $ "Unexpected result: " ++ show x


unpack_section_list :: [[SqlValue]] -> IO [Section]
unpack_section_list = return . map unpack_section

unpack_section_from_list :: [[SqlValue]] -> IO Section
unpack_section_from_list = return . head . map unpack_section


read_all_sections :: Connection -> IO [Section]
read_all_sections conn = quickQuery conn query [] >>= unpack_section_list where
  query = "SELECT * FROM section ORDER BY id"

read_section :: Id -> Connection -> IO Section
read_section section_id connection = quickQuery connection query params >>= unpack_section_from_list where
  query = "SELECT * FROM section WHERE id = ?"
  params = [SqlInteger section_id]

read_person_sections :: Id -> Connection -> IO [Section]
read_person_sections person_id connection = quickQuery connection query params >>= unpack_section_list where
  query = "SELECT * FROM section WHERE id IN (SELECT id FROM person WHERE id = ?)"
  params = [SqlInteger person_id]


update_section :: Id -> SectionTitle -> Connection -> IO Bool
update_section section_id section_title connection = 
  run connection query params >>= is_success_db_operation where 
    query = "UPDATE section SET title = ? WHERE id = ?"
    params = [SqlByteString $ BS.pack section_title, SqlInteger section_id]

delete_section :: Id -> Connection -> IO Bool
delete_section section_id connection = 
  run connection query params >>= is_success_db_operation where
    query = "DELETE FROM section WHERE id = ?"
    params = [SqlInteger section_id]


create_section :: SectionTitle -> Connection -> IO Bool
create_section section_title connection = 
  run connection query params >>= is_success_db_operation where 
    query = "INSERT INTO section (title) VALUES(?)" 
    params = [SqlByteString $ BS.pack section_title]