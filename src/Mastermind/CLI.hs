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

-- | Show the guess to the user in the terminal
displayGuess :: [Peg] -> IO ()
displayGuess guess = putStrLn $ "Try this guess: " ++ (show guess)

-- | Congratulate on the win or inform about the failure
finalReaction :: Bool -> IO ()
finalReaction True = putStrLn "Congrats on the win!"
finalReaction _    = putStrLn "No possibilities left. Something must have gone wrong."
