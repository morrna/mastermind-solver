module Main where

import Lib

main = do
    let numPegs = 4
    let numColors = 8
    let properPeg' = (properPeg numColors)
    let properGuess' = (properGuess numPegs numColors)
    let allG = allGuesses numPegs numColors

    let firstGuess = map (`mod` numColors) [ 0 .. (numPegs-1) ]

    putStrLn "starting Mastermind solver"
    putStrLn $ "This is for a game with " ++ (show numPegs)
            ++ " pegs to guess and " ++ (show numColors)
            ++ " distinct colors."

    win <- narrowGuesses firstGuess allG
    if win
        then putStrLn "Congrats on the win!"
        else putStrLn "Aw, what went wrong?"




-- Input is a guess plus the list of remaining possibilities
narrowGuesses :: [Int] -> [[Int]] -> (IO Bool)
narrowGuesses tryG oldposs = do
    putStrLn $ "Try this guess: " ++ (show tryG)
    putStrLn "How many red pegs did you get?"
    redStr <- getLine
    putStrLn "How many white pegs did you get?"
    whiteStr <- getLine
    let comp = ( (read redStr :: Int), (read whiteStr :: Int) )
    if comp == (4,0)
        then do
            putStrLn $ "There were " ++ (show $ length oldposs)
                    ++ " possibilities left."
            return True
        else do
            let newposs = filter (\gg -> guessComp tryG gg == comp) oldposs
            case newposs of
                headposs:_ -> narrowGuesses headposs newposs
                _ -> do
                    putStrLn "No possibilities remain."
                    return False

