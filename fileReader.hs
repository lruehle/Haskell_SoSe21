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
import qualified Data.ByteString.Lazy as BS
import GHC.Data.Maybe (fromJust, fromMaybe)
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
file = "Gedichte3.json"

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
        g = getIntro (sammlung !!(indx-1))
        h = getBody (sammlung !!(indx-1))
    in g ++ h


-- *** returns first unread Poem
firstUnread :: [Gedicht] -> String
firstUnread [] = "keine ungelesenen Gedichte"
firstUnread (x:xs)
    | "ausgelesen = False" `isInfixOf` show x = getIntro x ++ getBody x
    | otherwise = firstUnread xs

-- *** returns String with unread Poems 
listOfUnread :: [Gedicht] -> String
listOfUnread [] = []
listOfUnread (x:xs)
    | "ausgelesen = False" `isInfixOf` show x = "\n" ++ getIntro x ++ getBody x ++ listOfUnread xs
    | otherwise = listOfUnread xs

-- ********************** Create Random Number *******************

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


-- *** finds gedicht in Gedichte and increments Int with each recursion; c should always be 0 ; runtime block?
{-getIndex :: [Gedicht] -> Gedicht -> Int -> Int
getIndex a b c 
    | null a = -1
    | a!!c == b = c
    | otherwise = getIndex a b c+1 -}

-- *** returns the index of the first element in [String]/hay matching String/needle
getFirstIndex :: String -> [String] -> Int --needs f.e ["Eichendorf", "Ringelnatz"] als [String]
getFirstIndex needle hay = head $ elemIndices needle hay --returns a list of indices
--getIndex _ [] = -1
--getIndex a (x:xs) 
  --  | a == x = 

-- *** gibt liste mit ausgelesen = False zurück
ausgelesenToList :: [Gedicht] -> [String]
ausgelesenToList = map (show . ausgelesen) --show (ausgelesen x) : ausgelesenToList xs


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
    let poem = [Gedicht {autor=autor needle, titel=titel needle, gedicht=gedicht needle, jahr=jahr needle,ausgelesen= True,anzahlGelesen=anzahlGelesen needle+1}]
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
           putStrLn "parsing done"
           let listGedichte = gedichte parsed
           -- putStrLn ("hi \n" ++ show listGedichte)
           putStrLn ("\nWilkommen in der Gedichte Sammlung!\nZur Zeit stehen " ++ show (getLength listGedichte) ++ " Gedichte zur Auswahl \n")
           let length = getLength listGedichte
           putStrLn ("random Number "++ show(randomNumbInBound(dateNumber (date current)current) length))
           let rand = randomNumbInBound(dateNumber (date current) current) length
           putStrLn "Was möchten Sie tun?\nein zufälliges Gedicht lesen? (r) \t\t ein ungelesenes Gedicht lesen? (u)\t\t Gedichte eines Autors lesen? (a)"
           putStrLn $ "ausgelesen: "++ show(ausgelesenToList listGedichte)
           putStrLn $ "hi"++ show(getFirstIndex "False" (ausgelesenToList listGedichte))
           --putStrLn ( "bla"++show(getIndex listGedichte (listGedichte!!3) 0))
           user <- getChar
           let check | user == 'r' = do
                        let newJson = encode (changeAmountRead parsed rand)
                        BS.writeFile file2 newJson
                        putStr ("\nSuper, hier ein zufälliges Gedicht! Viel Spaß \n" ++ printGedichtAt listGedichte rand)
                     | user == 'u' = do
                        --let indx = getFirstIndex "False" (ausgelesenToList listGedichte)
                        let newJson = encode (changeAmountRead parsed (getFirstIndex "False" (ausgelesenToList listGedichte)))
                        BS.writeFile file2 newJson
                        putStr ("Hier ein bisher ungelesenes Gedicht" ++ firstUnread listGedichte)
                     | user == 'a' = putStr "Welchen Autor wollen sie gerne lesen?"
                     | user == 'n' = putStr "Sehr Schade, Sie verpassen was"
                     | otherwise = putStr "Ich lerne noch! Bisher verstehe ich nur:\n\t 'r' für ein zufälliges Gedicht \n\t 'u' für ein ungelesenes Gedicht \n\t 'a' für Gedichtausgabe nach Autor \n\t und 'n' für nichts"
           check
            --only for random Gedicht, set Index to random
           --print (changeAmountRead parsed rand)
           --BS.writeFile file2 newJson -- g3.json wird nicht aktualisiert ?




{-- next up:
    make random number from length
        ***-> make own random number
        ***-> make random number with Range of Length
        -> Error testing? Can runtime err occur?
    
    Write to an FromJson myself? --> !!! Encoded Json has different form from original (tags are a mess) 
    repeat questions in main (without repeating parse possible? reasonable? Repeat only right? Reasonable until poems can get added?)
    print choosable options to cmd possible? 
    pretty print function instead of adding "\n\t" to String
    add more choices (read out unread poem, read poem numb..., read poem by artist..)

    xxx get length of Gedicht -> Gedichte
    xxx print out Data at length
    xxx Change read 
    xxx print out more Data than just Titel & Autor
    xxx think about Poem formatting / reading / printing ; seperate by ,.;:? use \n ? no new line in JSON source accepted 
            for human readableness use List of Strings for Lines?
    ----make own "Date" - Type?
--}
