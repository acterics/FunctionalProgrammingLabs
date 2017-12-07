module Section where

import Prelude hiding (read)
import Database.HDBC
import Database.HDBC.PostgreSQL
import qualified Data.ByteString.Char8 as BS


data Section = Section {id :: Integer, title :: String} deriving (Show)
data SectionParticipation = SectionParticipation {section_id :: Id, person_id :: Id }



unpack_section :: [SqlValue] -> Section
unpack_section [SqlInteger section_id, SqlByteString section_title] =
  Section section_id (BS.unpack section_title)
unpack_section x = error $ "Unexpected result: " ++ show x


read_all_sections :: Connection -> IO [Section]
read_all_sections conn = quickQuery conn query [] >>= handle_result where
  query = "SELECT * FROM section ORDER BY id"
  handle_result result = return $ map unpack_section result