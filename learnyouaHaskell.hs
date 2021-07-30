--working through the learnYouAHaskell Book & creating hopefully usefull funcs on top while doing so
import System.IO

doubleMe x = x +x
doubleUs y x = x*2 + y*2

multby2If x = if x> 100
                then x
                else x * 2

lucky :: (Integral a) => a -> String 
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you're out of luck!"

head' :: [a] -> a
head' [] = error "empty, no head"
head' (x:_) = x

tell :: (Show a) => [a] -> String
tell [] = "the list is empty"
tell [x] = "one Element: " ++ show x 
tell [x,y] = "two Elements: " ++ show x ++ " " ++ show y 
tell (x:y:_) = "too many Elements! fst: " ++ show x ++ ", scnd:  " ++ show y 

length' :: (Num b) => [a] -> b
length' [] = 0 -- edge condition
length' (_:xs) = 1 + length' xs

sum' ::(Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs 

capital :: String -> String  
capital "" = "Empty string, whoops!"  
capital all@(x:xs) = "The first letter of " ++ all ++ " is " ++ [x]

bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  

max' :: (Ord a) => a -> a -> a
max' num1 num2 
    | num1 < num2     = num2 
    | otherwise       = num1

initials :: String -> String -> String
initials fst_name lst_name = [f] ++ ". " ++[l]++"."
    where (f:_) = fst_name
          (l:_) = lst_name

replicate' :: (Num i, Ord i) => i -> a ->[a]
replicate' n x 
    | n <= 0   = []
    | otherwise = x:replicate' (n-1) x

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0 = [] -- guard without otherwise part, so matching falls through
take' _ []   = []
take' n (x:xs) = x:take' (n-1) xs

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a >= x]
    in smallerSorted ++ [x] ++ biggerSorted

compareWithHundred :: (Num a, Ord a) => a -> Ordering  
compareWithHundred = compare 100 

zipWith' :: (a->b->c) -> [a] -> [b]-> [c]
zipWith' _ [] _ = [] 
zipWith' _ _ [] = [] 
zipWith' f (x:xs) (y:ys) = f x y :  zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c  
flip' f y x = f x y  

--lilz
printWithLineBreak = zipWith' (++) ["erster Satz","zweiter Satz","dritter Satz"] ["\n", " linebreak \n", " linebreak \n"]

--lilz
strListToString :: [String] -> String -- ["erster Satz","zweiter Satz linebreak","dritter Satz"] -- unlines does same check: https://hackage.haskell.org/package/base-4.15.0.0/docs/Prelude.html#v:unlines
strListToString [] = []
strListToString (x:xs) = x ++ strListToString xs --needs putStr to format in ghci
--listToString (x:xs) = x  ++ "\n" ++ listToString xs -- adds New Line, use delimiter func strListToStringWith

--lilz
strListToStringWith :: [String] -> String -> String --if no delimiter, use listToString
strListToStringWith [] _ = []
strListToStringWith [x] _ = x --last Element does not get \n
strListToStringWith (x:xs) delim = x ++ delim ++ strListToStringWith xs delim 

--lilz
strListToStringNL :: [String] -> String
strListToStringNL [] = []
strListToStringNL x = strListToStringWith x " \n " 
--lilz
strListCurry = strListToStringWith ["this is a curry!", "....or at least a try"] -- figuring out currying -- strListCurry ":))" => "this is a curry!:))....or at least a try"

--lilz
addNewLine :: String -> String
addNewLine a = a ++ "\n" 

-- Point of Lambda https://stackoverflow.com/questions/22220439/haskell-lambda-expression/22221169
addThree :: (Num a) => a -> a -> a -> a  
addThree x y z = x + y + z  
-- is equal to:
addThree' :: (Num a) => a -> a -> a -> a  
addThree'    = \x -> \y -> \z -> x + y + z


main = do
    contents <- readFile "Gedichte.json"
    putStr contents