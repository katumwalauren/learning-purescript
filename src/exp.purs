module  Main
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
import Prim.Boolean (False)

main :: Effect Unit
main = do
log $ show $ theLength wordOfStrings
log $ show $ getIt wordOfStrings
log $ show $ oddNum 5
log $ show $ theFiltered numArray
log $ show $ theFirst
log $ show $ inserted "Cultus"
log $ show $ replicated "mine"
log $ show $ findIt 6
log $ show $ multiply 3 3
log $ show $ thePrefix "Miss." "Katumwa"
log $ show $ theDrop wordOfStrings
log $ show $ theToUpper
log "🍝"

numArray :: Array Int 
numArray = [5 ,6, 7, 8]

wordOfStrings :: Array String
wordOfStrings = ["Lauren", "Chloe", "Andrew"]

--use filter
theFiltered :: Array Int -> Array Int
theFiltered array =  filter filtered array

filtered :: Int -> Boolean
filtered num = num > 5

--use length
theLength :: Array String -> Array Int
theLength array = map findLength array

findLength :: Array String -> Int
findLength new = String.length new

--use map
theMap :: Array Int  -> Array String
theMap array = map findIt array

findIt :: Int -> String 
findIt a = show $ a + 2

--use find
getIt :: Array String -> Maybe String 
getIt array = find getThat array

getThat :: String -> Boolean
getThat string = string =="Lauren"

--find odd returning a Boolean
oddNum :: Int -> Boolean
oddNum nums = if (nums / 2) == % 1 then True else False

-- use head
theFirst ::  Maybe String 
theFirst = head wordOfStrings

--use insert
inserted :: String -> Array String
inserted array = insert array wordOfStrings

--use replicate 
replicated :: Int -> Array String 
replicated array = replicate 7 array

-- elm
theElem  :: Array Int -> Boolean
theElem array = elem 6 array

--multiply 
multiply :: Int -> Int -> Int
multiply a b = a * b

--use prefix 
thePrefix :: String -> String -> String 
thePrefix prefix word = prefix <> word 

--use drop
theDrop :: Array String -> Array String
theDrop array = map showIt array

showIt :: String -> String 
showIt arrays = String.drop 3 arrays

--toUpper
theToUpper :: Array String
theToUpper array = map String.toUpper wordOfStrings

-- find Shortest


