module Lib
    ( properPeg
    , properGuess
    , guessComp
    , allGuesses
    ) where

import Data.List

properPeg :: Int -> Int -> Bool
properPeg numColors p = (p >= 0) && (p < numColors)

properGuess :: Int -> Int -> [Int] -> Bool
properGuess numPegs numColors g = (length g == numPegs)
                && (all (properPeg numColors) g )

allGuesses :: Int -> Int -> [[Int]]
allGuesses 0 _ = [[]]
allGuesses np nc = [ c:ps | c <- [0 .. (nc - 1)]
                            , ps <- (allGuesses (np-1) nc) ]

-- Utility stuff
-- probably need this because I don't know the libraries well enough.
countIn :: [Int] -> Int -> Int
countIn l v = length $ filter (v==) l

guessComp :: [Int] -> [Int] -> (Int, Int)
guessComp = curry $ guessesInexact . (uncurry guessesExact)
--guessComp h g = guessesInexact $ guessesExact h g

-- Takes two lists and returns the number that exactly match,
-- along with the elements left over.
guessesExact :: [Int] -> [Int] -> ([Int], [Int], Int)
guessesExact h g
    | (length h) /= (length g) = error "Compared guesses must have equal length."
guessesExact [] [] = ( [], [], 0 )
guessesExact (hh:ht) (gh:gt)
    | hh == gh = ( hl
                    , gl
                    , en + 1 )
    | otherwise = ( hh:hl
                    , gh:gl
                    , en )
    where ( hl, gl, en ) = guessesExact ht gt

-- WRONG
-- if h has two pegs the same, a single match in g will be double counted
guessesInexact :: ([Int], [Int], Int) -> (Int, Int)
guessesInexact (h, g, e) = let hn = nub h in
                           (e, sum $ zipWith min
                            (map (countIn g) hn) (map (countIn h) hn)
                           )
