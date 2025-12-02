{-|
module:         Mastermind.Pegs
Description:    Represent peg states in Mastermind
 -}
module Mastermind.Pegs (
    Peg(..),
    allStates,
) where

-- | Represent a peg as a number specifying its color.
--
-- This can be any int to allow for varying numbers of colors specified at run time.
newtype Peg = Peg { pegColor :: Int }
    deriving (Eq, Show)

-- | All possible combinations of pegs for a given game.
allStates
    :: Int -- ^ Total number of possible colors
    -> Int -- ^ Number of peg slots in each guess
    -> [[Peg]]
allStates _ 0 = [[]]
allStates nColor nPeg
    = [ (Peg color):otherPegs |
        otherPegs <- allStates nColor (nPeg-1),
        color <- [1..nColor]
    ]
