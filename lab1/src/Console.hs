module Console where


import Database.HDBC.PostgreSQL
import Person
import Prelude


show_person :: Person -> IO ()
show_person person = 
    putStrLn $ (showField person Person.id) ++ (showField person firstName) ++ (showField person lastName) ++ (showField person position) where 
        showField person field = show (field person) ++ "      "

show_persons :: Connection -> IO ()
show_persons connection = read_all_persons connection >>= show_persons_list

show_persons_list :: [Person] -> IO ()
show_persons_list persons = do
    putStrLn "################### PERSONS ###################"
    putStrLn "ID   | FIRST NAME  | LAST NAME | POSITION "
    show_persons_list' persons


show_persons_list' :: [Person] -> IO ()
show_persons_list' persons = do 
    mapM_ show_person persons
