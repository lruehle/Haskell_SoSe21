module Main where
    
import Data.Char
import Control.Applicative

data JsonValue = JsonNull
                |JsonBool Bool
                |JsonNumber Integer -- only int support
                |JsonString String
                |JsonArray [JsonValue]
                |JsonObject [( String, JsonValue)] --map included in Data; Containers
                deriving (Show, Eq)

newtype Parser a = Parser
    { runParser :: String -> Maybe (String, a)} --returns unparsed input & parsed; maybe for indicating errors (no detail) : data Maybe a = Just a| Nothing; a bit like NULL for Haskell

--make Functor; runParser (fmap ord $ charP 'n) "nice" returns: Just ("ice", 110) -> ascII of 'n'
instance Functor Parser where 
    fmap f (Parser p) = Parser $ \input -> do -- $ omits set of ()
                        (input', x) <- p input
                        Just (input', f x)

-- make Parser Applicative, has to go after Functor, has to support <*>
instance Applicative Parser where 
    pure x = Parser $ \input -> Just(input, x) 
    (Parser p1) <*> (Parser p2) =
        Parser $ \input ->do -- chains Parsers & their output as enxt input
            (input', f) <- p1 input 
            (input'', a) <- p2 input'
            Just (input'', f a)
    -- <$> operator : equal to fmap
    -- <*> operator: (+) <$> Just 5  <*> Just 6 output: Just 11; (,,,) <$> Just 1 <*> Just 2 <*> Just 3 output: Just (1,2,3)

--make Parser Alternative, so you can use <|>
instance Alternative Parser where 
    empty = Parser $ \_ -> Nothing 
    (Parser p1) <|> (Parser p2) = Parser $ \input -> 
                                    p1 input <|> p2 input --chooses the one that isn't failing; puts input into first parser, if fails, put input into 2nd parser

jsonNull :: Parser JsonValue
jsonNull = (\_ ->JsonNull) <$> stringP "null" -- replace whats in Parser with JsonNUll-obj; runParser jsonNull "null" output: Just ("", JsonNull)



--parser that parses single character
{- charP :: Char -> Parser Char  -- runParser (charP 'n') "nice" output: Just ("ice", 'n')
charP x = 
    Parser $ \input -> 
        case input of
        y:ys
          | y == x -> Just (ys, x) -- input has to be of expected value
        _ -> Nothing -}

--second Version, but same as charP
charP :: Char -> Parser Char 
charP x = Parser f 
    where 
        f(y:ys)
            | y == x = Just (ys, x)
            | otherwise = Nothing
        f[] = Nothing

stringP :: String -> Parser String
stringP = sequenceA . map charP -- '.' chains functions
-- sequenceA turns (Traversable t, Applicative f) t(f a) into f(t a) -> need to make Parser applicative (lists are traversable)  ->so that you have a Parser of a List

jsonBool :: Parser JsonValue
jsonBool = f <$> (stringP "true" <|> stringP "false") -- alternative operator <|> (picks first not empty alternative or Nothing)
    where f "true" = JsonBool True --penetrate to return JsonValue instead of [char]
          f "false" = JsonBool False
          f_        = undefined -- failsave; shouldn't happen though...

-- introduces span to the parser
spanP :: (Char -> Bool) -> Parser String
spanP f =  Parser $ \input -> 
    let (token, rest) = span f input --span function: one list -> two lists
    in Just (rest, token)

notNull :: Parser [a] -> Parser[a] -- checking original Parser if not empty
notNull (Parser p) =
    Parser $ \input -> do
    (input', xs) <- p input
    if null xs 
        then Nothing
        else Just (input', xs)

jsonNumber:: Parser JsonValue
jsonNumber = f <$> notNull (spanP isDigit)
    where f ds = JsonNumber $ read ds


stringLiteral :: Parser String
stringLiteral = charP '"' *> spanP (/= '"') <* charP '"' --everything that isn't " (first & last, doesn't support escaping)


jsonString :: Parser JsonValue
jsonString = JsonString <$> (charP '"' *> stringLiteral <* charP '"') -- will make sure it starts with " and endes with", will only return StringLiteral


ws:: Parser String
ws = spanP isSpace

sepBy :: Parser a -> Parser b -> Parser [b] 
sepBy sep element = (:) <$> element <*> many (sep *> element) 
    <|> pure [] --keyword many;pure 

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charP '[' *> ws *> elements <* ws <* charP ']' )
    where elements = sepBy (ws *> charP ',' <* ws) jsonValue
       -- sep = ws *> charP ',' <* ws


jsonObject :: Parser JsonValue
jsonObject = 
    JsonObject <$> (charP '{' *> ws *> sepBy (ws *> charP ',' <* ws) pair <* ws <* charP '}' ) -- pairs seperated by ,
    where pair = 
            (\key _ value -> (key, value)) <$> stringLiteral <*> (ws *>  charP ':' <* ws) <*> jsonValue


jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject


parseFile ::FilePath -> Parser a -> IO (Maybe a)
parseFile fileName parser = do
    input <- readFile fileName
    return (snd <$> runParser parser input)


main :: IO()
main = undefined