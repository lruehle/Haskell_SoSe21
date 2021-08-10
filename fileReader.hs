{-# LANGUAGE DeriveGeneric #-} -- https://hackage.haskell.org/package/aeson-1.5.6.0/docs/Data-Aeson.html
{-# LANGUAGE OverloadedStrings #-} --byteString from Aeson to String
-- {-# LANGUAGE RecordWildCards #-}


import System.IO
import System.Exit
import GHC.Generics
import Data.Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Aeson.Text (encodeToLazyText)
import qualified Data.Text as DT
import Data.Time
import Data.Time.Clock.POSIX
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate
import Data.List
import Data.Map (fromList)
import qualified Data.ByteString.Lazy as BS
import GHC.Data.Maybe (fromJust, fromMaybe, listToMaybe)
import GHC.Tc.Solver.Monad (getInertCans)





data Gedichte = Gedichte { -- https://artyom.me/aeson#nested-records check for nested json reading
    gedichte :: [Gedicht]
} deriving (Show, Generic, Eq)

data Gedicht = Gedicht {
        autor :: String,
        titel :: String,
        gedicht :: [String],
        jahr :: Integer,
        ausgelesen :: Bool,
        anzahlGelesen :: Int
    } deriving (Show, Generic, Eq)

data Keys = Au | Ti | Ged | Jahr | Ausg | Anz deriving (Show,Eq)
data Pair = Keys String deriving (Show, Eq)

instance FromJSON Gedicht
{-instance FromJSON Gedicht where
    parseJSON = withObject "gedicht" $ \o -> do
    autor <- o .: "autor"
    titel <- o .: "titel"
    gedicht <- o .: "gedicht"
    jahr <- o .: "jahr"
    ausgelesen <- o .: "ausgelesen"
    anzahlGelesen <- o .: "anzahlGelesen"
    let intro = "\n\t" ++ autor ++ " - "++ show jahr ++ " \n\t" ++ titel ++"\n"
    return Gedicht{..} -}
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

-- *** prints Gedicht Intro & body at Index 
printGedichtAt :: [Gedicht] -> Int -> String
printGedichtAt sammlung indx =
    let --f = getLength (gedichte sammlung)
        g = getIntro (sammlung !!indx)
        h = getBody (sammlung !!indx)
    in g ++ h

{-- obsolete ?
-- *** returns first unread Poem
firstUnread :: [Gedicht] -> String
firstUnread [] = "keine ungelesenen Gedichte"
firstUnread (x:xs)
    | "ausgelesen = False" `isInfixOf` show x = getIntro x ++ getBody x
    | otherwise = firstUnread xs


-- *** returns String with all unread Poems 
listOfUnread :: [Gedicht] -> [String]
listOfUnread [] = []
listOfUnread (x:xs)
    | "ausgelesen = False" `isInfixOf` show x = show("\n" ++ getIntro x ++ getBody x) : listOfUnread xs
    | otherwise = listOfUnread xs -}

-- ********************** Create Random Number *******************

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



deleteDoubles :: (Eq a, Ord a) => [a] -> [a]
deleteDoubles = map head . group . sort --same as : map head (group (sort a)) 



-- *** returns the index of the first element in [String]/hay matching String/needle
getFirstIndex :: String -> [String] -> Maybe Int --needs f.e ["Eichendorf", "Ringelnatz"] als [String]
getFirstIndex needle hay = listToMaybe $ elemIndices needle hay -- returns List of indices of String in List, and takes first Element from it



-- *** gibt liste mit "ausgelesen"-werten zurück => ["True","True","False","True","False",...]
ausgelesenToList :: [Gedicht] -> [String]
ausgelesenToList = map (show . ausgelesen) --show (ausgelesen x) : ausgelesenToList xs


keyToList :: [Gedicht] -> String -> [String]
keyToList [] _ = []
keyToList (x:xs) needle = case needle of
    "autor" ->   autor x : keyToList xs needle
    "titel" ->  titel x : keyToList xs needle
    --"gedicht" ->  gedicht x : keyToList xs needle --gedicht ist List
    "jahr" ->   show (jahr x) : keyToList xs needle
    "ausgelesen" ->  show ( ausgelesen x) : keyToList xs needle
    "anzahlGelesen" ->  show ( anzahlGelesen x) : keyToList xs needle
    _-> []
    


-- *** fallOverAndDie code from : https://stackoverflow.com/questions/14159855/how-to-implement-early-exit-return-in-haskell 
fallOverAndDie :: String -> IO a
fallOverAndDie err = do putStrLn err
                        exitWith (ExitFailure 1)




-- ********************** Change Data in Json File *******************

-- *** changes ausgelesen to True & increments anzahlGelesen by 1
    -- cuts List into Head and Tail at Index, copies old Element with new Values, sticks all parts back together
changeAmountRead :: Gedichte -> Int ->Gedichte
changeAmountRead oldJson indx = do
    let haystack = gedichte oldJson
    let needle = haystack !!indx -- printRandGedicht printed auch bei index-1
    let poem = [Gedicht {autor=autor needle, titel=titel needle, gedicht=gedicht needle, jahr=jahr needle,ausgelesen= True,anzahlGelesen=anzahlGelesen needle+1}]
    let headStack = Prelude.take indx haystack -- cut off till needle
    let tailStack = Prelude.drop (indx+1) haystack -- cut of after needle
    --let newstack = mergeLists headStack (mergeLists poem tailStack)
    let newstack = headStack ++ poem ++ tailStack -- ++ not so good for very long lists, when is it very long?
    let newJson = Gedichte newstack
    newJson




-- ********************** Main *******************
main :: IO ()
main = do --https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
    current <- getCurrentTime -- not TimeZoned
    parsedData <- eitherDecode <$> jsonData :: IO (Either String Gedichte) -- without IO throws error; reminder <$> is short for fmap, check on Functors
    case parsedData of --https://stackoverflow.com/questions/46944347/how-to-get-value-from-either   otherwise will return "Left ..." or "Right..."
       Left err -> fallOverAndDie ("Parsing Error! Programm will be stopped because: "++err)
       Right parsed -> do
           let listGedichte = gedichte parsed
           putStrLn $ "\nWilkommen in der Gedichte Sammlung!\nZur Zeit stehen " ++ show (getLength listGedichte) ++ " Gedichte zur Auswahl \n"
           let length = getLength listGedichte
           let rand = randomNumbInBound(dateNumber (date current) current) length
           putStrLn "Was möchten Sie tun?\n\t\t ein zufälliges Gedicht lesen? (r) \n\t\t ein ungelesenes Gedicht lesen? (u)\n\t\t Gedichte eines Autors lesen? (a)"
           user <- getLine
           let check | user == "r" = do
                        let newJson = encode (changeAmountRead parsed rand)
                        BS.writeFile file2 newJson
                        putStrLn ("\nSuper, hier ein zufälliges Gedicht! Viel Spaß! \n" ++ printGedichtAt listGedichte rand)
                     | user == "u" = do
                        let indx = getFirstIndex "False" (ausgelesenToList listGedichte)
                        case indx of
                            Just i ->do
                                putStrLn ("hi  "++ show i)
                                let newJson = encode (changeAmountRead parsed i)
                                BS.writeFile file2 newJson
                                --putStrLn ("Hier ein bisher ungelesenes Gedicht" ++ firstUnread listGedichte)
                                putStr ("Hier ein bisher ungelesenes Gedicht" ++ printGedichtAt listGedichte i)
                            Nothing -> putStrLn "Alle Gedichte wurden bereits Gelesen :)"
                     | user == "a" = do
                        putStrLn "Welchen Autor wollen sie gerne lesen?"
                        autor <- getLine
                        putStrLn $ "Moment, ich suche nach Gedichten von " ++ autor
                        let indx2 = getFirstIndex autor (keyToList listGedichte "autor")
                        --print indx2
                        case indx2 of
                            Just i -> do 
                                --putStrLn $"autorIndex"++ show(keyToList listGedichte "autor")
                                let newJson = encode (changeAmountRead parsed i)
                                BS.writeFile file2 newJson
                                putStr ("Hier ein Gedicht von: "++ autor ++ "\n" ++ printGedichtAt listGedichte i)
                            Nothing -> putStrLn $"Kein Gedicht von: " ++ autor ++ " gefunden\nZur Auswahl stehen:" ++ strListToStringWith (deleteDoubles $ keyToList listGedichte "autor") "\n\t" ++ "\n"
                     | user == "n" = putStr "Sehr Schade, Sie verpassen was"
                     | otherwise = putStr "Ich lerne noch! Bisher verstehe ich nur:\n\t 'r' für ein zufälliges Gedicht \n\t 'u' für ein ungelesenes Gedicht \n\t 'a' für Gedichtausgabe nach Autor \n\t und 'n' für nichts"
           check


{-- next up:
    
    
    Write to and FromJson myself? --> !!! Encoded Json has different form from original (tags are a mess/tags are sorted in alphabetic order) 
    repeat questions in main (without repeating parse possible? reasonable? Repeat only right? Reasonable until poems can get added?)
    
    add more choices (read out unread poem, read poem numb..., read poem by artist..)
        *** -> get unread single
            -> get unread plural
        *** -> get author single
            -> get author plural
            -> get author by firstname OR lastname

    Sprach-Tag hinzufügen (für Filter)? -> on Top: Gedichte mehrsprachig ermöglichen?



    done
    xxx get length of Gedicht -> Gedichte
    xxx print out Data at length
    xxx Change read 
    xxx print out more Data than just Titel & Autor
    xxx think about Poem formatting / reading / printing ; seperate by ,.;:? use \n ? no new line in JSON source accepted 
            for human readableness use List of Strings for Lines?
    xxx make random number from length
        ***-> make own random number
        ***-> make random number with Range of Length
        -> Error testing? Can runtime err occur?
    xxx replace FirstUnread function by PrintGedicht (getFirstIndex ausgelesenToList)
    xxx check out indices (-1 etc) -> better way?
    
    maybe

    ----print choosable options to cmd possible? 
    ----pretty print function instead of adding "\n\t" to String; Formatting
    ----cancel Case Sensitivity 
--}
