module Main (main) where

import Text.IO (readUntilSucceed)
import qualified Mastermind.Pegs as Pegs
import Mastermind.Pegs (
        Peg(..),
        Feedback(..),
        getFeedback,
    )
import qualified Mastermind.CLI as CLI
import Mastermind.Solver (
        GameParams(..),
        Interface(..),
        run,
    )


main = do
    let numPegs = 4
    let numColors = 8
    let interface = Interface {
            gameParams = GameParams { numColors = numColors, numPegs = numPegs }
        ,   displayGuess = CLI.displayGuess
        ,   askUserFeedback = CLI.readFeedback
        }

    CLI.putStartMessage numColors numPegs

    win <- run interface
    CLI.finalReaction win
