{-|
Module:         Mastermind.CLI
Description:    IO logic needed to run the solver in the CLI
 -}
module Mastermind.CLI (
    putStartMessage,
    readFeedback,
    displayGuess,
    finalReaction,
) where

import Data.Char (chr)

import Text.IO (readUntilSucceed)
import Mastermind.Pegs (
        Peg(..),
        Feedback(..),
    )

-- | Print a starting message with the game parameters to the terminal
putStartMessage
    :: Int
    -> Int
    -> IO ()
putStartMessage numColors numPegs = do
    putStrLn "starting Mastermind Solver"
    putStrLn $ "    for a game with " ++ (show numPegs)
            ++ " pegs to guess and " ++ (show numColors)
            ++ " distinct colors."

-- | Ask the user what feedback they got for their guess
readFeedback :: IO Feedback
readFeedback = do
    fRed <- readUntilSucceed "How many red pegs did you get?"
    fWhite <- readUntilSucceed "How many white pegs did you get?"
    return $ Feedback fRed fWhite

-- | Show pegs as unicode colored balls (as much as possible)
instance Show Peg where
    show (Peg 1) = "\x1F534" -- red circle
    show (Peg 2) = "\x1F7E2" -- green circle
    show (Peg 3) = "\x1F535" -- blue circle
    show (Peg 4) = "\x1F7E1" -- yellow circle
    show (Peg 5) = "\x1F7E4" -- brown circle
    show (Peg 6) = "\x1F7E3" -- purple circle
    show (Peg 7) = "\x2B24" -- black circle
    show (Peg 8) = "\x25EF" -- white circle
    show (Peg 9) = "\x1F7E0" -- orange circle
    show (Peg p)
        | p <= 20   = [chr (9311+p)]
        | otherwise = "(" ++ show p ++ ")"


-- | Show the guess to the user in the terminal
displayGuess :: [Peg] -> IO ()
displayGuess guess = putStrLn $ "Try this guess: " ++ (show guess)

-- | Congratulate on the win or inform about the failure
finalReaction :: Bool -> IO ()
finalReaction True = putStrLn "Congrats on the win!"
finalReaction _    = putStrLn "No possibilities left. Something must have gone wrong."
