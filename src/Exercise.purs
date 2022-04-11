module Exercise
  ( Person
  , fullName
  , getFullName
  , girlChild
  , kenya
  , persons
  , setAge
  , thatPerson
  , uganda
  )
  where

import Prelude
import Math
import Data.Array
import Data.Maybe
import Data.Ord
import Prelude

import Data.String as String
import Effect (Effect)
import Effect.Console (log)
import Prim.Boolean (False)

type Country =
 { president :: String
 , city :: String 
 , population :: Int 
 }

uganda :: Country
uganda = 
 { president : "Museveni"
 , city : "kampala"
 , population : 30
 }

kenya :: Country 
kenya = 
 { president :"uhuru"
 , city : "Nirobi"
 , population : 90
 }


type Person =
 { firstName :: String
 , lastName :: String 
 , age :: Int
 }

thatPerson :: Person
thatPerson = 
 { firstName : "Katumwa"
 , lastName : "Lauren" 
 , age : 19
 }

girlChild :: Person 
girlChild =
 { firstName : "Nalubega"
 , lastName : "Cultus"
 , age : 14
 }

persons :: Array Person
persons = [thatPerson, girlChild]

--get fullName of person

fullName :: Person -> String
fullName person = person.firstName <> person.lastName

--set age
setAge :: Int -> Person -> Person 
setAge newAge person = person { age = newAge }

--get all fullName

getFullName :: Array Person -> Array String
getFullName array = map fullName array


-- find the average age