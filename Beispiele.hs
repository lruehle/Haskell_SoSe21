import System.IO
import GHC.Data.Maybe 
import qualified Helpers as H

-- Beispiel 1 - Lazyness:

f1 :: Maybe a -> [Maybe a]
f1 m = [m,m]
-- -> f1 needs to know nothing about m

f2 :: Maybe a -> [a]
f2 Nothing  = []
f2 (Just x) = [x]

-- -> f2 needs to know something about a (Nothing or Just)
{- 
-- => bsp: f2 (Just $5^2)
--       Ergebnis: f2 Just(5^2) => [5^2], ohne Berechnung von 5^2 
         Berechnung z.b nachdem f2 mit show aufgerufen wird

         Pattern Matching Berechnet Wert, aber nur insoweit wie nötig
         ->s. Helpers.hs "getLength" 81-84

-}

endless :: [Integer]
endless = take 3 $ repeat 5



-- Beispiel 2 - Lazyness
f :: Num a => a -> a -> a
f x y = x +2  -- + y -- vgl mit/ohne +y

f' :: Integer
f' = f 5 (3^35792956) -- ziemlich Große Nummer




-- Beispiel 3 
{-  Applicative vs. Normal Order

(define (p) (p))
(define (test x y)
  (if (= x 0)
    0
    y))
    
(test 0 (p))

applicative: endless loop, because (p) just calls (p), and (p) has to be evaluated before if is evaluated
normal Order: 0 is returned, because If is evaluated before else (= x 0)
-}



-- Beispiel 4 - Currying
maxOr15 :: Integer -> Integer
maxOr15 = max 15
{-
    maxOr15 2 => 15
    maxOr15 22 => 22
-}


--or
deleteDouble' :: [Integer]
deleteDouble' = H.deleteDoubles [1,3,4,2,1,3]
