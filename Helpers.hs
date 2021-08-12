
module Helpers(
    date,
    dateNumber,
    randomNumbInBound,
    strListToString,
    strListToStringWith,
    crossSum,
    deleteDoubles,
    getFirstIndex,
    getAllIndices,
    noticeMe,
    getLength,
    fallOverAndDie
)where

import System.Exit
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.List
import GHC.Data.Maybe (fromJust, fromMaybe, listToMaybe)



date :: UTCTime -> (Integer,Int,Int) --Year is type Integer
date = toGregorian .utctDay -- will output f.e (2021,8,5)

-- *** if delimiter needed use strListToStringWith
strListToString :: [String] -> String -- ["erster Satz","zweiter Satz linebreak","dritter Satz"] -- unlines does same check: https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:unlines
strListToString [] = []
strListToString (x:xs) = x ++ strListToString xs --needs putStr to format in ghci


-- *** if no delimiter, use listToString
strListToStringWith :: [String] -> String -> String
strListToStringWith [] _ = []
--strListToStringWith [x] _ = x --last Element does not get \n
strListToStringWith (x:xs) delim = delim ++ x ++ strListToStringWith xs delim

-- *** Queersumme
crossSum :: Int -> Int
crossSum 0 = 0
crossSum num = mod num 10 + crossSum (div num 10) --mod gets last digit as 1983 % 10 =3


-- *** deletes all doubles in a List by: sorting List, grouping equal adjecents into new Lists, then maps "head" to the List
deleteDoubles :: (Eq a, Ord a) => [a] -> [a]
deleteDoubles = map head . group . sort --same as : map head (group (sort a)) 


-- *** returns the index of the first element in [String]/hay matching String/needle
getFirstIndex :: String -> [String] -> Maybe Int --needs f.e ["Eichendorf", "Ringelnatz"] als [String]
getFirstIndex needle hay = listToMaybe $ elemIndices needle hay -- returns List of indices of String in List, and takes first Element from it


-- *** returns List with Indices of all Elements in [String]/hay (f.e.["Eichendorf","Ringelnatz"]) matching String/needle (f.e "Eichendorf")
getAllIndices :: String -> [String] -> [Int]
getAllIndices = elemIndices -- returns List of indices of String in List
-- same as: getAllIndices needle [hay] = elemIndices needle [hay]


-- *** generate random Number from Date & Time
dateNumber :: (Integer,Int,Int) -> UTCTime ->Int
dateNumber (y,m,d) time = a + b + c + e
    where a = fromIntegral(diffTimeToPicoseconds (utctDayTime time))
          b =  d
          c =  m
          e =  fromIntegral y


-- *** check if num/Queersumme small enough for bound; bound is length of list => List indx + 1
randomNumbInBound :: Int -> Int -> Int
randomNumbInBound numb bound
    | numb == 0 = 0 --ever reached?
    | numb <= bound = numb-1 --numb is now == indx
    | otherwise = randomNumbInBound (crossSum numb) bound --get crossSum until small enough


-- *** returns Length of an array
getLength :: [a] -> Int
getLength [] = 0
getLength (_:xs) = 1 + getLength xs


-- *** adds formatting to String
noticeMe:: String -> String
noticeMe a= "\n    ..........................................\n " ++ a ++"\n    ..........................................\n"
--noticeMe a= "\n    ******************************************\n " ++ a ++"\n    ******************************************\n"
--noticeMe a= "\n\t -------------------------\n " ++ a ++"\n\t -------------------------\n"


-- *** fallOverAndDie code from : https://stackoverflow.com/questions/14159855/how-to-implement-early-exit-return-in-haskell 
fallOverAndDie :: String -> IO a
fallOverAndDie err = do putStrLn err
                        exitWith (ExitFailure 1)
