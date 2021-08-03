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
        gedicht :: String,
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
getIntro inst = autor inst ++ " : " ++ titel inst

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
    in "Autor und Titel sind: " ++ g
                -- can i call do recursion with different input types? if parameter int =; if parameter string= etc?


{-main = do
    contents <- readFile "Gedichte.json" -- use Data.ByteString.Lazy.readFile is better?
    putStr contents -}

-- fallOverAndDie code from : https://stackoverflow.com/questions/14159855/how-to-implement-early-exit-return-in-haskell 
fallOverAndDie :: String -> IO a
fallOverAndDie err = do putStrLn err
                        exitWith (ExitFailure 1)


main :: IO ()
main = do --https://www.schoolofhaskell.com/school/starting-with-haskell/libraries-and-frameworks/text-manipulation/json
    --content <- contents
    --putStr content --works because IO box is now opened ?
    --print contents -- doesn't work because of ´unopenable´ IO Box? IO is not function, but action. So we need to run it before printing. <- basically is "run" operator 
    -- print jsonDate 
    --parsedData <- (eitherDecode <$> jsonData) :: IO (Either String [Gedicht]) --gedichte3.json

    parsedData <- eitherDecode <$> jsonData :: IO (Either String Gedichte) -- without IO throws error; reminder <$> is short for fmap, check on Functors
    case parsedData of --https://stackoverflow.com/questions/46944347/how-to-get-value-from-either   otherwise will return "Left ..." or "Right..."
       Left err -> fallOverAndDie ("Parsing Error! Programm will be stopped because: "++err)
       --Right parsed -> print parsed -- print (and not Strln because print calls show to turn into String)
       --Right parsed -> print ( gedichte parsed) -- prints gedichte from parsed (::Gedichte)
       --Right parsed -> print ( getIntro (gedichte parsed)) -- prints getIntro for (gedichte parsed), nur bei non-Array Child Object von Gedicht   
       --Right parsed -> print ( getIntro (gedichte parsed!!1) ++ show(getLength (gedichte parsed))) -- prints getIntro for (gedichte (parsed index 1)) -- remember index 0 is head
       Right parsed -> do 
           print "parsing done" --print $ printRandGedicht parsed -- put into variable but NOT print possible?
           putStrLn "Wilkommen in der Gedichte Sammlung"
           print ("Zur Zeit stehen " ++ show (getLength(gedichte parsed)) ++ " Gedichte zur Auswahl")
           print "Möchten Sie ein zufälliges Gedicht lesen?"
           user <- getLine
           let check | user == "y" = putStr ("Super! \n" ++ show (printRandGedicht parsed))
                     | user == "n" = putStr "Sehr Schade, Sie verpassen was"
                     | otherwise = putStr "Ich lerne noch! Bisher verstehe ich nur 'y' für ja und 'n' für nein"
           check
           


{-- next up:
    get length of Gedicht -> Gedichte
    make random number from length
    print out Data at length
    print out more Data than just Titel & Autor
    Change read 
--} 
