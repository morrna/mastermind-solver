{-|
module:         Mastermind.Pegs
Description:    Represent peg states in Mastermind
 -}
module Mastermind.Pegs (
    Peg(..),
    allStates,
    Feedback(..),
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

-- | Represents the red and white pegs used to give feedback on a guess
data Feedback
    = Feedback {
        reds :: Int, -- ^ # pegs matching color and position
        whites :: Int -- ^ # pegs matching color but not position
    }
    -- | Since the number of colors is specified at runtime, the compiler needs something in case list lengths mismatch.
    | InvalidFeedback
    deriving (Show, Eq)

instance Semigroup Feedback where
    (Feedback r1 w1) <> (Feedback r2 w2) = Feedback (r1+r2) (w1+w2)
    InvalidFeedback <> _ = InvalidFeedback
    _ <> InvalidFeedback = InvalidFeedback

instance Monoid Feedback where
    mempty = Feedback 0 0
