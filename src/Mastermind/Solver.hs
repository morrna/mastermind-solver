{-|
Module:         Mastermind.Solver
Description:    High level logic for zeroing in on a game solution
 -}
module Mastermind.Solver (
    GameParams(..),
    Interface(..),
    run,
) where

import Data.Maybe (
        listToMaybe,
    )

import Mastermind.Pegs (
        Peg(..),
        Feedback(..),
        allStates,
        getFeedback,
    )

data GameParams = GameParams
    { numColors :: Int
    , numPegs   :: Int
    }
    deriving (Eq, Show)

data Interface m = Interface
    { gameParams        :: GameParams
    , displayGuess      :: [Peg] -> m ()    -- ^ Display a guess for the user to use
    , askUserFeedback   :: m Feedback       -- ^ Gather the feedback received from the guess
    }

-- | Take functions on numColors and numPegs and apply them to values from the interface
onGameParams
    :: Monad m
    => (Int -> Int -> r) -- ^ Function taking number of colors as first arg and number of pegs as second
    -> Interface m
    -> r
onGameParams f = (f <$> numColors <*> numPegs) . gameParams

-- | Run the game solution
--
-- This just iterates toward a final solution. Introducing the game and its parameters, for example,
-- belongs outside this.
run :: Monad m
    => Interface m
    -> m Bool       -- ^ Returns True for wins and False for losses or errors
run interface = do
    narrow
        interface
        []

-- | Take steps to narrow
narrow :: Monad m
    => Interface m
    -> [([Peg], Feedback)]  -- ^ Guesses so far
    -> m Bool
narrow interface guessSoFar = do
        let tryNextGuess = listToMaybe $ filter checkGSF allGameStates
        case tryNextGuess of
            Nothing -> return False
            Just nextGuess -> do
                displayGuess interface nextGuess
                nextFeedback <- askUserFeedback interface
                if isWin nextFeedback
                then return True
                else narrow interface $ (nextGuess, nextFeedback):guessSoFar
    where
        allGameStates = onGameParams allStates interface
        matchGuess g1 (g2, fb) = fb == getFeedback g1 g2
        checkGSF g1 = all (matchGuess g1) guessSoFar
        isWin fb = (reds fb == (numPegs . gameParams) interface)
