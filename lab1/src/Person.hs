module Person where

import Prelude hiding (read)
import Database.HDBC
import Database.HDBC.PostgreSQL

type Id = Integer
type FirstName = String
type LastName = String
data Position = Student | Teacher deriving (Enum)

data Person = Person {id :: Id, firstName :: FirstName, lastName :: LastName, position :: Position}


helloworld = putStrLn "Hello World"