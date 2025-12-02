module Main where

import Lib
import Text.IO (readUntilSucceed)
import qualified Mastermind.Pegs as Pegs
import Mastermind.Pegs (
        Peg(..),
        Feedback(..),
        getFeedback,
    )

main = do
    let numPegs = 4
    let numColors = 8
    let allG = Pegs.allStates numColors numPegs

    let firstGuess = map ((+1) . (`mod` numColors)) [ 0 .. (numPegs-1) ]

    putStrLn "starting Mastermind solver"
    putStrLn $ "This is for a game with " ++ (show numPegs)
            ++ " pegs to guess and " ++ (show numColors)
            ++ " distinct colors."

    win <- narrowGuesses firstGuess $ (Pegs.pegColor <$>) <$> allG
    if win
        then putStrLn "Congrats on the win!"
        else putStrLn "Aw, what went wrong?"




-- Input is a guess plus the list of remaining possibilities
narrowGuesses :: [Int] -> [[Int]] -> (IO Bool)
narrowGuesses tryG oldposs = do
    putStrLn $ "Try this guess: " ++ (show tryG)
    fRed <- readUntilSucceed "How many red pegs did you get?"
    fWhite <- readUntilSucceed "How many white pegs did you get?"
    let comp = Feedback fRed fWhite
    -- flag: `4` should be `numPegs` - refactor to not hard code
    if comp == Feedback 4 0
        then do
            putStrLn $ "There were " ++ (show $ length oldposs)
                    ++ " possibilities left."
            return True
        else do
            let newposs = filter (\gg -> getFeedback (Peg <$> tryG) (Peg <$> gg) == comp) oldposs
            case newposs of
                headposs:_ -> narrowGuesses headposs newposs
                _ -> do
                    putStrLn "No possibilities remain."
                    return False

