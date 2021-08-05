{-# LANGUAGE DeriveGeneric #-} -- https://hackage.haskell.org/package/aeson-1.5.6.0/docs/Data-Aeson.html
{-# LANGUAGE OverloadedStrings #-} --byteString from Aeson to String


import System.IO
import System.Exit
import Data.Aeson
import GHC.Generics
import Data.Text
import qualified Data.ByteString.Lazy as BS
import GHC.Data.Maybe (fromJust)
import Data.Aeson.Types (parseMaybe)
import GHC.Tc.Solver.Monad (getInertCans)
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate



data Person = Person {
    name :: String
    , age  :: Int
    } deriving (Generic, Show)

instance ToJSON Person where 
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Person where  

-- read Datas into Tuples? Each Gedicht is a Tuple, Gedichte therefore a List of Tuples? -> Size stays the same, but mixed value
data Gedichte = Gedichte { -- https://artyom.me/aeson#nested-records check for nested json reading
    gedichte :: [Gedicht]
} deriving (Show, Generic)  --Gedichte2.json

data Gedicht = Gedicht {
        autor :: String,
        titel :: String,
        gedicht :: [String],
        jahr :: Integer,
        ausgelesen :: Bool,
        ausgelesenAm :: String -- check for Date Time Format
    } deriving (Show, Generic)

-- Write to an FromJson myself?
instance FromJSON Gedicht
instance ToJSON Gedicht

instance FromJSON Gedichte --Gedichte2.json
instance ToJSON Gedichte

getChild :: Gedichte -> Int -> Gedicht
getChild parent index =  undefined

getIntro :: Gedicht -> String
getIntro inst = "\n" ++ autor inst ++ " - "++ show (jahr inst) ++ " \n" ++ titel inst ++"\n"

getBody :: Gedicht -> String
getBody inst = "\n" ++ strListToStringWith (gedicht inst) "\n" 

getLength :: [a] -> Int
getLength [] = 0
getLength (_:xs) = 1 + getLength xs

file :: FilePath
file = "Gedichte2.json"
--file = "Gedichte3.json"

jsonData :: IO BS.ByteString
jsonData = BS.readFile file

contents :: IO String
contents = readFile file




-- if delimiter needed use strListToStringWith
strListToString :: [String] -> String -- ["erster Satz","zweiter Satz linebreak","dritter Satz"] -- unlines does same check: https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:unlines
strListToString [] = []
strListToString (x:xs) = x ++ strListToString xs --needs putStr to format in ghci
--listToString (x:xs) = x  ++ "\n" ++ listToString xs -- adds New Line, 

--if no delimiter, use listToString
strListToStringWith :: [String] -> String -> String 
strListToStringWith [] _ = []
strListToStringWith [x] _ = x --last Element does not get \n
strListToStringWith (x:xs) delim = x ++ delim ++ strListToStringWith xs delim 


printRandGedicht :: Gedichte -> String
printRandGedicht sammlung = 
    let f = getLength (gedichte sammlung)
        g = getIntro (gedichte sammlung !!(f-1))-- replace by random number
        h = getBody (gedichte sammlung !! (f-1)) --replace by random number
    in g ++ h   --"\n " ++ g ++ "\n" ++ h ++ "\n" --why is linebreak not working? now in g & h
                -- can i call do recursion with different input types? if parameter int =; if parameter string= etc?


{-main = do
    contents <- readFile "Gedichte.json" -- use Data.ByteString.Lazy.readFile is better?
    putStr contents -}

-- fallOverAndDie code from : https://stackoverflow.com/questions/14159855/how-to-implement-early-exit-return-in-haskell 
fallOverAndDie :: String -> IO a
fallOverAndDie err = do putStrLn err
                        exitWith (ExitFailure 1)


date :: UTCTime -> (Integer,Int,Int) --Year is type Integer
date = toGregorian .utctDay -- will output f.e (2021,8,5)





randomNumber :: (Integer,Int,Int) -> UTCTime ->Int
--randomNumber (y,m,d) time = fromIntegral(diffTimeToPicoseconds (utctDayTime time)) + m + d + fromIntegral y
randomNumber (y,m,d) time = div (a + b + c + e ) 2
    where a = fromIntegral(diffTimeToPicoseconds (utctDayTime time))
          b =  d
          c =  m
          e =  fromIntegral y
{--
get day, get Time, get time since runghc started (or other "random" values)
put into Range of 0, getLength (Gedichte Gedicht)
--}


main :: IO ()
main = do --https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
    --content <- contents
    --putStr content --works because IO box is now opened ?
    --print contents -- doesn't work because of ´unopenable´ IO Box? IO is not function, but action. So we need to run it before printing. <- basically is "run" operator 
    -- print jsonDate 
    --parsedData <- (eitherDecode <$> jsonData) :: IO (Either String [Gedicht]) --gedichte3.json
    current <- getCurrentTime -- not TimeZoned
    --let PicosecondsSince = diffTimeToPicoseconds (utctDayTime current) --10^-12 for Pico to second
    parsedData <- eitherDecode <$> jsonData :: IO (Either String Gedichte) -- without IO throws error; reminder <$> is short for fmap, check on Functors
    case parsedData of --https://stackoverflow.com/questions/46944347/how-to-get-value-from-either   otherwise will return "Left ..." or "Right..."
       Left err -> fallOverAndDie ("Parsing Error! Programm will be stopped because: "++err)
       --Right parsed -> print parsed -- print (and not Strln because print calls show to turn into String)
       --Right parsed -> print ( gedichte parsed) -- prints gedichte from parsed (::Gedichte)
       --Right parsed -> print ( getIntro (gedichte parsed)) -- prints getIntro for (gedichte parsed), nur bei non-Array Child Object von Gedicht   
       --Right parsed -> print ( getIntro (gedichte parsed!!1) ++ show(getLength (gedichte parsed))) -- prints getIntro for (gedichte (parsed index 1)) -- remember index 0 is head
       Right parsed -> do 
           putStrLn "parsing done" --print $ printRandGedicht parsed -- put into variable but NOT print possible?
           putStrLn ("Wilkommen in der Gedichte Sammlung! \n Zur Zeit stehen " ++ show (getLength(gedichte parsed)) ++ " Gedichte zur Auswahl \n")
           putStrLn ("Sekunden seit Mittnacht: "++ show current)  
           putStrLn ("Sekunden seit Mittnacht: "++ show (diffTimeToPicoseconds (utctDayTime current)))  
           putStrLn ("random Number "++ show (randomNumber (date current) current))  
           putStrLn ("datum ist: "++ show (date current))  
           putStrLn "Möchten Sie ein zufälliges Gedicht lesen? (y/n)"
           user <- getLine
           let check | user == "y" = putStr ("Super, viel Spaß! \n" ++ printRandGedicht parsed)
                     | user == "n" = putStr "Sehr Schade, Sie verpassen was"
                     | otherwise = putStr "Ich lerne noch! Bisher verstehe ich nur 'y' für ja und 'n' für nein"
           check
           



{-- next up:
    make random number from length
        ***-> make own random number
        -> make random number with Range of Length
    Change read 
    make own "Date" - Type?
    repeat questions in main (without repeating parse possible? reasonable? Repeat only right? Reasonable until poems can get added?)
    print choosable options to cmd possible? 
    xxx get length of Gedicht -> Gedichte
    xxx print out Data at length
    xxx print out more Data than just Titel & Autor
    xxx think about Poem formatting / reading / printing ; seperate by ,.;:? use \n ? no new line in JSON source accepted 
            for human readableness use List of Strings for Lines?
--} 
