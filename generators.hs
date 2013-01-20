-- http://www.cis.uoguelph.ca/~sawada/papers/chord.pdf
-- http://www.cis.uoguelph.ca/~sawada/papers/un.pdf
-- http://www.cis.uoguelph.ca/~sawada/papers/alph.pdf

import Control.Monad.Reader
import Data.List (intersperse)

type AlphabetLength = Int
type TargetLength = Int
type LyndonNumber = Int
type Word = [Int]

alph = 2 -- magic constant representing alphabet length
         -- I would add it into the reader, but I never change it

prenecklaces = filterPrelist (\_ -> const True) -- prelist generates exactly prenecklaces
lyndonWords  = filterPrelist (==) -- lyndon number equals length <=> the word is lyndon
necklaces    = filterPrelist (\n p -> n `mod` p == 0) -- straightforward

-- Generates list of words with their lyndon numbers, tupled like (word, lyndonNumber)
-- initial tuple should be -- duh! -- ([0], 1): smallest non-empty lyndon word
prelist :: (Word, LyndonNumber) -> 
             Reader (TargetLength, AlphabetLength) [(Word, LyndonNumber)]
prelist (word, p) = ask >>= \(n, k) ->
  let t = length word
  in  if t == n
        then return [(word,p)]
        else let alph = [word !! (t - p) + 1 .. k-1]
                 fn i = prelist ( word ++ [i]              , t+1 )
                 a1   = prelist ( word ++ [word !! (t - p)], p   )
                 a2   = mapM fn alph >>= return . concat 
             in do x <- a1
                   y <- a2
                   return $ x ++ y

filterPrelist :: (Int -> Int -> Bool) -> Int -> String
filterPrelist fn n = mpr $ filterPrelist' fn n

filterPrelist' :: (Int -> Int -> Bool) -> Int -> [Word]
filterPrelist' fn n =
  let l = runReader (prelist ([0], 1)) (n,alph) ++ [(replicate n 1, 1)]
      fn1 (word, lyndon) = fn (length word) lyndon
  in map fst $ filter fn1 l

runPrelist :: TargetLength -> String
runPrelist n = mshp $ runReader (prelist ([0], 1)) (n, alph)

shp :: (Word, LyndonNumber) -> String
shp (w,p) = pr w ++ ":" ++ show p

mshp :: [(Word, LyndonNumber)] -> String
mshp a = concat $ intersperse ", " $ map shp a

pr :: (Show a) => [a] -> String
pr a = foldl (++) "" (map show a)

mpr :: (Show a) => [[a]] -> String
mpr a = concat $ intersperse ", " $ map pr a
