module Lib
    ( properPeg
    , properGuess
    , guessComp
    , allGuesses
    ) where

properPeg :: Int -> Int -> Bool
properPeg numColors p = (p >= 0) && (p < numColors)

properGuess :: Int -> Int -> [Int] -> Bool
properGuess numPegs numColors g = (length g == numPegs)
                && (all (properPeg numColors) g )

intEq :: Int -> Int -> Int
intEq i j
    | i == j = 1
    | i /= j = 0

guessComp :: [Int] -> [Int] -> (Int, Int)
guessComp g h
    | length g /= length h = error "Guesses must be the same length!"
guessComp g h = (sum $ zipWith intEq g h
                , sum [ (g !! i) `intEq` (h !! j)
                        | i <- [0 .. (length g - 1)]
                        , j <- [0 .. (length h - 1)]
                        , i /= j
                        , (g !! i) /= (h !! i)
                        , (g !! j) /= (h !! j)
                    ]
                )

allGuesses :: Int -> Int -> [[Int]]
allGuesses 0 _ = [[]]
allGuesses np nc = [ c:ps | c <- [0 .. (nc - 1)]
                            , ps <- (allGuesses (np-1) nc) ]
