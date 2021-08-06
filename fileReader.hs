{-# LANGUAGE DeriveGeneric #-} -- https://hackage.haskell.org/package/aeson-1.5.6.0/docs/Data-Aeson.html
{-# LANGUAGE OverloadedStrings #-} --byteString from Aeson to String


import System.IO
import System.Exit
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Aeson.Text (encodeToLazyText)
import Data.Text 
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import qualified Data.ByteString.Lazy as BS
import GHC.Data.Maybe (fromJust)
import GHC.Tc.Solver.Monad (getInertCans)




data Person = Person {
    name :: String
    , age  :: Int
    } deriving (Generic, Show)

instance ToJSON Person where 
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Person where  


data Gedichte = Gedichte { -- https://artyom.me/aeson#nested-records check for nested json reading
    gedichte :: [Gedicht]
} deriving (Show, Generic) 

data Gedicht = Gedicht {
        autor :: String,
        titel :: String,
        gedicht :: [String],
        jahr :: Integer,
        ausgelesen :: Bool,
        anzahlGelesen :: Int 
    } deriving (Show, Generic)


instance FromJSON Gedicht
instance ToJSON Gedicht

instance FromJSON Gedichte 
instance ToJSON Gedichte



file :: FilePath
file = "Gedichte2.json"

file2 :: FilePath
file2 = "Gedichte3.json"




-- ********************** Get Json contents *******************

jsonData :: IO BS.ByteString
jsonData = BS.readFile file

getIntro :: Gedicht -> String
getIntro inst = "\n\t" ++ autor inst ++ " - "++ show (jahr inst) ++ " \n\t" ++ titel inst ++"\n"

getBody :: Gedicht -> String
getBody inst = "\n " ++ strListToStringWith (gedicht inst) "\n \t" 

getLength :: [a] -> Int
getLength [] = 0
getLength (_:xs) = 1 + getLength xs



-- ********************** Create Random Number *******************

printRandGedicht :: Gedichte -> Int -> String
printRandGedicht sammlung indx = 
    let --f = getLength (gedichte sammlung)
        g = getIntro (gedichte sammlung !!(indx-1))
        h = getBody (gedichte sammlung !!(indx-1)) 
    in g ++ h  


-- *** generate random Number from Date & Time
dateNumber :: (Integer,Int,Int) -> UTCTime ->Int
dateNumber (y,m,d) time = a + b + c + e 
    where a = fromIntegral(diffTimeToPicoseconds (utctDayTime time))
          b =  d
          c =  m
          e =  fromIntegral y

-- *** check if num/Queersumme small enough for bound
randomNumbInBound :: Int -> Int -> Int
randomNumbInBound numb bound 
    | numb <= bound = numb
    | otherwise = randomNumbInBound (crossSum numb) bound --get crossSum until small enough
 


-- ********************** Helper Functions *******************


date :: UTCTime -> (Integer,Int,Int) --Year is type Integer
date = toGregorian .utctDay -- will output f.e (2021,8,5)

-- *** if delimiter needed use strListToStringWith
strListToString :: [String] -> String -- ["erster Satz","zweiter Satz linebreak","dritter Satz"] -- unlines does same check: https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:unlines
strListToString [] = []
strListToString (x:xs) = x ++ strListToString xs --needs putStr to format in ghci
--listToString (x:xs) = x  ++ "\n" ++ listToString xs -- adds New Line, 

-- *** if no delimiter, use listToString
strListToStringWith :: [String] -> String -> String 
strListToStringWith [] _ = []
--strListToStringWith [x] _ = x --last Element does not get \n
strListToStringWith (x:xs) delim = delim ++ x ++ strListToStringWith xs delim 

-- *** Queersumme
crossSum :: Int -> Int
crossSum 0 = 0
crossSum num = mod num 10 + crossSum (div num 10) --mod gets last digit as 1983 % 10 =3

{-- try finding more efficience than ++
mergeLists :: [a] -> [a] -> [a]
mergeLists [] [] = []
mergeLists [] y = y
mergeLists x []  = x
--mergeLists (x:xs) ys = x: mergeLists xs ys 
mergeLists [x] [y] = [x, y] --}


-- *** fallOverAndDie code from : https://stackoverflow.com/questions/14159855/how-to-implement-early-exit-return-in-haskell 
fallOverAndDie :: String -> IO a
fallOverAndDie err = do putStrLn err
                        exitWith (ExitFailure 1)



-- ********************** Change Data in Json File *******************
changeAmountRead :: Gedichte -> Int ->Gedichte
changeAmountRead oldJson indx = do
    let haystack = gedichte oldJson
    let needle = haystack !!(indx-1) -- printRandGedich printed auch bei index-1
    let poem = [Gedicht {autor=autor needle, titel=titel needle, gedicht=gedicht needle, jahr=jahr needle,ausgelesen= ausgelesen needle,anzahlGelesen=anzahlGelesen needle+1}]
    let headStack = Prelude.init(Prelude.take indx haystack) -- returns List without last item
    let tailStack = Prelude.drop indx haystack
    --let newstack = mergeLists headStack (mergeLists poem tailStack)
    let newstack = headStack ++ poem ++ tailStack -- ++ not so good for very long lists, when is it very long?
    let newJson = Gedichte newstack
    newJson
        {--
        get list of old Gedichte oldJson
        get Gedicht at ListIndex indx from oldJson
        make new Gedicht with newAmountRead
        make new List same as oldJson, but with replaced Gedicht&NewAmountRead at Index
        return Gedichte
        or write Gedichte to file
        --}




-- ********************** Main *******************
main :: IO ()
main = do --https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
    current <- getCurrentTime -- not TimeZoned
    --let PicosecondsSince = diffTimeToPicoseconds (utctDayTime current) --10^-12 for Pico to second
    parsedData <- eitherDecode <$> jsonData :: IO (Either String Gedichte) -- without IO throws error; reminder <$> is short for fmap, check on Functors
    case parsedData of --https://stackoverflow.com/questions/46944347/how-to-get-value-from-either   otherwise will return "Left ..." or "Right..."
       Left err -> fallOverAndDie ("Parsing Error! Programm will be stopped because: "++err)
       Right parsed -> do 
           putStrLn "parsing done" --print $ printRandGedicht parsed -- put into variable but NOT print possible?
           putStrLn ("Wilkommen in der Gedichte Sammlung! \n Zur Zeit stehen " ++ show (getLength(gedichte parsed)) ++ " Gedichte zur Auswahl \n")
           let length = getLength(gedichte parsed)
           --putStrLn ("Sekunden seit Mittnacht: "++ show current)  
           --putStrLn ("Sekunden seit Mittnacht: "++ show (diffTimeToPicoseconds (utctDayTime current)))  
           putStrLn ("random Number "++ show(randomNumbInBound(dateNumber (date current) current) length)) 
           let rand = randomNumbInBound(dateNumber (date current) current) length
           --putStrLn ("datum ist: "++ show (date current))  
           putStrLn "Möchten Sie ein zufälliges Gedicht lesen? (y/n)"
           user <- getLine
           let check | user == "y" = putStr ("\n Super, viel Spaß! \n" ++ printRandGedicht parsed rand)
                     | user == "n" = putStr "Sehr Schade, Sie verpassen was"
                     | otherwise = putStr "Ich lerne noch! Bisher verstehe ich nur 'y' für ja und 'n' für nein"
           check
           let newJson = encode (changeAmountRead parsed rand)
           --print (changeAmountRead parsed rand)
           BS.writeFile file2 newJson -- g3.json wird nicht aktualisiert ?
           



{-- next up:
    make random number from length
        ***-> make own random number
        ***-> make random number with Range of Length
        -> Error testing? Can runtime err occur?
    
    Write to an FromJson myself? --> !!! Encoded Json has different form from original (tags are a mess) 
    repeat questions in main (without repeating parse possible? reasonable? Repeat only right? Reasonable until poems can get added?)
    print choosable options to cmd possible? 
    pretty print function instead of adding "\n\t" to String
    

    xxx get length of Gedicht -> Gedichte
    xxx print out Data at length
    xxx Change read 
    xxx print out more Data than just Titel & Autor
    xxx think about Poem formatting / reading / printing ; seperate by ,.;:? use \n ? no new line in JSON source accepted 
            for human readableness use List of Strings for Lines?
    ----make own "Date" - Type?
--} 
