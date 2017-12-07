module UpdateCommand where

import Lib

process :: [String] -> IO Integer
process arguments = do
    putStrLn $ show arguments
    return continue_code