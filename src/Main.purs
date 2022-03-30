module Main
  ( arrayOfStrings
  , main
  , newOne
  , reduceOne
  , theReplicate
  )
  where


import Data.Array
import Data.Maybe
import Data.Ord
import Prelude

import Data.String as String
import Effect (Effect)
import Effect.Console (log)

main :: Effect Unit
main = do
  log $ show $ sum 1 2
  log $ addPrefix "mr." "Lukwago"
  log $ isOld 25
  log $ show $ addOne 4
  log $ show $ length someArray
  log $ show $ haveInset "Alex"
  log $ show $ theReplicate "by"
  log $ show $ theDropped  arrayOfStrings
  log $ show $ findThat arrayOfStrings 
  log $ show $ firstOne
  log $ show $ theCapital
  log $ show $ shortestString arrayOfStrings
  log $ show $ line 2 4 
  --log $ isEven 8
  log $ show $ findLength arrayOfStrings
  log "🍝"

sum :: Int -> Int -> Int 
sum a b = a + b
  
  -- prefix
addPrefix :: String -> String -> String 
addPrefix prefix word = prefix <> word 

-- use if else
isOld :: Int -> String 
isOld age = if age > 20 then "This person is old" else "This person is young" 

someArray :: Array Int 
someArray = [1, 2]

arrayOfStrings :: Array String 
arrayOfStrings = ["allan", "betty", "Ben"]

--ue map
changeToString :: Array Int -> Array String 
changeToString array = map addOne array

-- map :: (Int -> String) -> Array Int -> Array String
-- map :: (a -> b) -> f a -> f b


addOne :: Int -> String
addOne x = show $ x + 1 

-- use insert Array function 
haveInset :: String ->  Array String
haveInset value = insert value arrayOfStrings

-- use filter
reduceOne :: Array Int -> Array Int
reduceOne array = filter newOne array

newOne :: Int -> Boolean
newOne num = num > 1

-- use head
firstOne ::  Maybe String
firstOne = head arrayOfStrings

-- use elem function 
givenElement :: Array Int -> Boolean
givenElement array = elem 5 array


-- use find 
findThat :: Array String -> Maybe String
findThat array = find word array

word :: String -> Boolean
word string =  string == "betty"

--use replicate
theReplicate :: String -> Array String
theReplicate value = replicate 5 value 


-- use the toUpper function from Data.String and array map function
-- to capitalize all letters in an arrayofStrings
theCapital :: Array String 
theCapital = map String.toUpper arrayOfStrings


-- Use the drop function from Data.String to remove the first 2 letters 
-- off each string, in an arrayofstrings.
theDropped :: Array String -> Array String
theDropped words = map dropLetters words

dropLetters :: String -> String 
dropLetters word' = String.drop 2 word'


-- find shortest string

shortestString :: Array String -> Maybe String
shortestString array = head (sortArray array) 

sortArray :: Array String -> Array String
sortArray array = sortBy compareLength array

compareLength :: String -> String -> Ordering 
compareLength x y = compare (String.length x) (String.length y)


-- write a function named called 'lineFunction or line' that replicates this formular 
-- y = x * a + c 
-- where 'x' and 'a' are function arguments and 'c' is constant that is equal to 10 
line :: Int -> Int -> Int
line x a = (x * a) + 10


-- write a function named 'isEven' that checks whether a function is even or not. If its
-- even return boolean value 'True'; 'False' otherwise

--isEven :: Int -> Boolean
--isEven num = if (num / 2) == 0 then True else False

-- Write a function that takes in an arrayofstrings and returns the length of each string
findLength :: Array String -> Array Int
findLength array = map theLength array

theLength :: String -> Int
theLength nums = String.length nums
 
