-- Process 'delete' command
module CreateCommand(process) where

import Lib
import Database.HDBC.PostgreSQL (Connection)

process :: Connection -> [String] -> IO Integer



process _ args = show_error "create" args