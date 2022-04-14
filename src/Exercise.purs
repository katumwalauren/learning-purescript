module Exercise
  ( Person
  , Result(..)
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

import Data.Array
import Data.Maybe
import Data.Ord
import Math
import Prelude
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
--sum
--find
--getAverage :: Array Person -> Person
--getAverage person = sum findAge person

--findAge :: String -> Boolean
--findAge  

-- sort persons by age and return youngest person
--sortby
--head
youngestPerson :: Array Person -> Maybe Person
youngestPerson persons = head (sortPersonsByAge persons) 

sortPersonsByAge :: Array Person -> Array Person 
sortPersonsByAge persons' = sortBy comparePersonsByAge persons'

comparePersonsByAge :: Person -> Person -> Ordering 
comparePersonsByAge person1 person2 = compare person1.age person2.age

--wen age is alredy got or sorted
sortPersonsByAge' :: Array Person -> Array Int 
sortPersonsByAge' persons = sortAge (map getAge persons)
 
getAge :: Person -> Int 
getAge person = person.age  

sortAge :: Array Int -> Array Int 
sortAge ages  = sortBy compareLength ages

compareLength :: Int -> Int -> Ordering 
compareLength x y = compare x y 

--use addable
class Addable a where 
  addable :: a -> a -> a 

instance Addable Int where 
  addable x y = x + y 

-- implement type class instance for Number type 
class Addable' a where 
  addable' :: a -> a -> a 

instance Addable' Number where
  addable' c d = c + d

--use show 
class Show' a where 
  show' :: a -> String 

instance Show' String where 
  show' x = x 

-- implement Show' instance for Int 

instance Show' Int where 
  show' c = show c

  --use joinable
class Joinable a where 
  join :: a -> a -> a 

-- implement Joinable instance for string 
class Joinable' a where 
  join' :: a -> a -> a 

instance Joinable' String where   
  join' add word = add <> word 

   
-- write a function that sorts persons by the length of their firstName
-- such that you return the person with the shortest firsName
shortestNameOfPerson :: Array Person -> Maybe Person
shortestNameOfPerson persons = head (sortPersonsByShortestName persons) 

sortPersonsByShortestName :: Array Person -> Array Person 
sortPersonsByShortestName persons'' = sortBy comparePersonsByShortestName persons''

comparePersonsByShortestName :: Person -> Person -> Ordering 
comparePersonsByShortestName personA personB = compare personA.firstName personB.firstName


data Maybe' a = Nothing' | Just' a

data Animal = Mammal | Reptile | Bird | Domestic | Wild


data Result a = Success a | Failure String 

isItReptile :: Animal -> Boolean
isItReptile animal = case animal of 
  Mammal -> false 
  Reptile -> true 
  Bird -> false 
  Wild -> false
  Domestic -> false

-- isReptile animal 

animal :: Animal 
animal = Mammal 

bird :: Animal 
bird = Bird 

--- using case statements write a show instance for Animals 

class IsWild where 
  isWild :: Animal -> Boolean

instance Show Animal where 
  show animal = case animal of 
    Reptile -> "reptile"
    Wild -> "wild"
    Domestic -> "domestic"
    Mammal -> "mammal"
    Bird -> "bird"

  --isBird animal
reptile :: Animal 
reptile = Reptile

domestic :: Animal
domestic = Domestic


data Orderable' = EQ | LT | GT 

class Ord' a where 
  compare' :: a -> a -> Orderable'

instance Ord' Int where 
   compare' x y 
      | x > y     = GT 
      | x < y     = LT 
      | otherwise = EQ 

instance Ord' Animal where 
  compare' x y = case [x, y] of 
    [Wild, Domestic] -> GT 
    [Mammal, Wild]   -> LT 
    _otherwise        -> EQ

-- Ord instance for String 
instance Ord' String where 
   compare' x y 
     | String.length x > String.length y = GT 
     | String.length x < String.length y = LT
     | otherwise                         = EQ

class Eq' a where 
  eq' :: a -> a -> Boolean 

instance Eq' Int where 
  eq' x y 
    | x == y = true  
    | otherwise = false 

-- Eq instance for String 
instance Eq' String where
  eq' x y
   | String.length x > String.length y = true
   | otherwise                         = false

-- Eq instance for Animal type
instance Eq' Animal where 
  eq' x y = case [x, y] of 
    [Wild, Domestic] -> true 
    _otherwise        -> false