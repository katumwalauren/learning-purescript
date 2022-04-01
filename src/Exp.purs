module Exp
  ( filtered
  , findIt
  , findLength
  , getIt
  , getThat
  , inserted
  , myMain
  , multiply
  , numArray
  , oddNum
  , replicated
  , showIt
  , theDrop
  , theElem
  , theFiltered
  , theFirst
  , theLength
  , thePrefix
  , theToUpper
  , wordOfStrings
  )
  where

import Math
import Data.Array
import Data.Maybe
import Data.Ord
import Prelude

import Data.String as String
import Effect (Effect)
import Effect.Console (log)
import Prim.Boolean (False)

myMain :: Effect Unit
myMain = do
  log $ show $ theLength wordOfStrings
  log $ show $ getIt wordOfStrings
  log $ show $ oddNum 5.0
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

findLength :: String -> Int
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
oddNum :: Number -> Boolean
oddNum nums = if (nums % 2.0) /= 0.0  then true else false

-- use head
theFirst ::  Maybe String 
theFirst = head wordOfStrings

--use insert
inserted :: String -> Array String
inserted array = insert array wordOfStrings

--use replicate 
replicated :: String -> Array String 
replicated number = replicate 7 number

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
theToUpper ::  Array String
theToUpper  = map String.toUpper wordOfStrings

-- find Shortest


