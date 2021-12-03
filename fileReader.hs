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
import Data.Time.Clock.POSIX

import Data.List
import Data.Map (fromList)
import qualified Data.ByteString.Lazy as BS
import GHC.Data.Maybe (fromJust, fromMaybe, listToMaybe)
import GHC.Tc.Solver.Monad (getInertCans)
import Text.Read (readMaybe)
-- own modules
import qualified Helpers as H





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
file = "Gedichte3.json" 

file2 :: FilePath
file2 = "Gedichte3.json"




-- ********************** Get Json contents *******************

jsonData :: IO BS.ByteString
jsonData = BS.readFile file

getIntro :: Gedicht -> String
getIntro inst = "\n\t" ++ autor inst ++ " - "++ show (jahr inst) ++ " \n\t" ++ titel inst ++"\n"

getBody :: Gedicht -> String
getBody inst = "\n " ++ H.strListToStringWith (gedicht inst) "\n \t"


-- *** prints Gedicht Intro & body at Index 
printGedichtAt :: [Gedicht] -> Int -> String
printGedichtAt sammlung indx =
    let
        g = getIntro (sammlung !!indx)
        h = getBody (sammlung !!indx)
    in H.noticeMe $ g ++ h

printGedichteAt :: [Gedicht] -> [Int] -> [String]
printGedichteAt sammlung = map (printGedichtAt sammlung)


-- ********************** Helper Functions *******************

-- *** returns a List with all the data Parts of a key 
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





-- ********************** Change Data in Json File *******************


-- *** changes ausgelesen to True & increments anzahlGelesen by 1
    -- cuts List into Head and Tail at Index, copies old Element and changes Values, sticks all parts back together
changeAmountRead :: Gedichte -> Int ->Gedichte
changeAmountRead oldJson indx = do
    let haystack = gedichte oldJson
    let needle = haystack !!indx 
    let poem = [Gedicht {autor=autor needle, titel=titel needle, gedicht=gedicht needle, jahr=jahr needle,ausgelesen= True,anzahlGelesen=anzahlGelesen needle+1}]
    let headStack = Prelude.take indx haystack -- cut off till needle
    let tailStack = Prelude.drop (indx+1) haystack -- cut of after needle
    let newstack = headStack ++ poem ++ tailStack -- ++ not so good for very long lists, when is it very long?
    let newJson = Gedichte newstack
    newJson

-- ** changes Read Value for Array of indices
changeAmountsRead :: Gedichte -> [Int] -> Gedichte
changeAmountsRead json [] = json
changeAmountsRead json [x] = changeAmountRead json x
changeAmountsRead json (x:xs) = changeAmountsRead (changeAmountRead json x) xs     -- should be only ONE file at the end with ALL of the new values




-- ********************** Main *******************
main :: IO ()
main = do --https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
    current <- getCurrentTime -- not TimeZoned
    parsedData <- eitherDecode <$> jsonData :: IO (Either String Gedichte) -- without IO throws error; reminder <$> is short for fmap, check on Functors
    case parsedData of --https://stackoverflow.com/questions/46944347/how-to-get-value-from-either   otherwise will return "Left ..." or "Right..."
       Left err -> H.fallOverAndDie ("Parsing Error! Programm will be stopped because: "++err)
       Right parsed -> do
           let listGedichte = gedichte parsed
           let length = H.getLength listGedichte
           putStrLn $ "\nWilkommen in der Gedichte Sammlung!\nZur Zeit stehen " ++ show length ++ " Gedichte zur Auswahl \n"  --  foldr (\_ n -> 1 + n) 0
           let rand = H.randomNumbInBound(H.dateNumber (H.date current) current) length
           putStrLn "Was möchten Sie tun?\n\t\t ~ Ein zufälliges Gedicht lesen? (r) \n\t\t ~ Gedicht an Stelle x lesen? (i)\n\t\t ~ Ein ungelesenes Gedicht lesen? (u)\n\t\t ~ Gedichte eines speziellen Autors lesen? (a) \n\t\t ~ Die Anwendung verlassen (q)"
           user <- getLine
           let check | user == "r" = do
                        let newJson = encode (changeAmountRead parsed rand)
                        BS.writeFile file2 newJson
                        putStrLn ("\nSuper, hier ein zufälliges Gedicht an Stelle "++show rand++"! Viel Spaß! \n" ++ printGedichtAt listGedichte rand)
                    | user == "i" = do
                        putStrLn $ "\nAlles klar. Das wievielte Gedicht soll ausgelesen werden? (1-" ++ show length ++ ")"
                        input <- getLine
                        let intInput = readMaybe input :: Maybe Int
                        case intInput of
                            Just i -> do
                                 let newJson = encode (changeAmountRead parsed $i-1)
                                 BS.writeFile file2 newJson
                                 putStrLn ("\nSuper, hier das " ++ show i ++". Gedicht! Viel Spaß! \n" ++ printGedichtAt listGedichte (i-1))
                            Nothing -> putStrLn("\nBitte eine ganze Zahl zwischen 1 und "++show length ++" eingeben!")
                     | user == "u" = do
                        let indx = H.getFirstIndex "False" $ keyToList listGedichte "ausgelesen"--(ausgelesenToList listGedichte)
                        case indx of
                            Just i ->do
                                --putStrLn ("hi  "++ show i)
                                let newJson = encode (changeAmountRead parsed i)
                                BS.writeFile file2 newJson
                                --putStrLn ("Hier ein bisher ungelesenes Gedicht" ++ firstUnread listGedichte)
                                putStrLn $ "Hier ein bisher ungelesenes Gedicht" ++ printGedichtAt listGedichte i
                            Nothing -> putStrLn  "Alle Gedichte wurden bereits Gelesen :)\n"
                     | user == "a" = do
                        putStrLn "Welchen Autor wollen sie gerne lesen?"
                        autor <- getLine
                        putStrLn $ H.noticeMe $"Moment, ich suche nach Gedichten von " ++ autor
                        let keys = keyToList listGedichte "autor"
                        let indx_many = H.getAllIndices autor keys
                        case indx_many of
                            [] -> putStrLn $"Kein Gedicht von: " ++ autor ++ " gefunden\nZur Auswahl stehen:" ++ H.strListToStringWith (H.deleteDoubles keys) "\n\t| " ++ "\n"
                            [x] -> do
                                let newJson = encode (changeAmountRead parsed x)
                                BS.writeFile file2 newJson
                                putStrLn $"Hier ein Gedicht von: "++ autor ++ "\n" ++ printGedichtAt listGedichte x
                            (x:xs) -> do
                                   --let read = changeAmountsRead parsed indx_many
                                   let newJson = encode $changeAmountsRead parsed indx_many
                                   BS.writeFile file2 newJson
                                   putStrLn $"Super! Ich habe sogar mehrere Gedichte von "++ autor ++ "\n" ++ H.strListToStringWith (printGedichteAt listGedichte indx_many) "\n\t"
                     | user == "q" = do
                        putStr "Auf wiedersehen!"
                        exitSuccess
                     | otherwise = putStr "Entschuldigung, ich lerne noch! Bisher verstehe ich nur:\n\t 'r' für ein zufälliges Gedicht \n\t 'i' für ein Gedicht an x-ter Stelle  \n\t 'u' für ein ungelesenes Gedicht \n\t 'a' für Gedichtausgabe nach Autor \n\t und 'q' zum Beenden der Anwendung"
           check

















{-- next up:
    
    
    Write to and FromJson myself? --> !!! Encoded Json has different form from original (tags are a mess/tags are sorted in alphabetic order) 
    repeat questions in main (without repeating parse possible? reasonable? Repeat only right? Reasonable until poems can get added?)
    
    add more choices (read out unread poem, read poem numb..., read poem by artist..)
        *** -> get unread single
            -> get unread plural
        *** -> get author single
        *** -> get author plural
            -> get author by firstname OR lastname

    Sprach-Tag hinzufügen (für Filter)? -> on Top: Gedichte mehrsprachig ermöglichen?
    *** ausgelesenToList -> KeytoList
            -> Paramaters as Data
    practice pointfree <-> pointfull conversion


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



{-- obsolete methods ?

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
    | otherwise = listOfUnread xs 
    
    
-- *** gibt liste mit "ausgelesen"-werten zurück => ["True","True","False","True","False",...]
ausgelesenToList :: [Gedicht] -> [String]
ausgelesenToList = map (show . ausgelesen) --show (ausgelesen x) : ausgelesenToList xs
    
    -}