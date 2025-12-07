{-|
module:         Mastermind.Pegs
Description:    Represent peg states in Mastermind
 -}
module Mastermind.Pegs (
    Peg(..),
    allStates,
    Feedback(..),
    getFeedback,
) where

-- | Represent a peg as a number specifying its color.
--
-- This can be any int to allow for varying numbers of colors specified at run time.
newtype Peg = Peg { pegColor :: Int }
    deriving (Eq)

-- | All possible combinations of pegs for a given game.
allStates
    :: Int -- ^ Total number of possible colors
    -> Int -- ^ Number of peg slots in each guess
    -> [[Peg]]
allStates nColor nPeg = allStatesInternal nColor nPeg 0

-- | Set up the set with staggering to more quickly probe possibilities
allStatesInternal :: Int -> Int -> Int -> [[Peg]]
allStatesInternal _ 0 _ = [[]]
allStatesInternal nColor nPeg offset
    = [ (Peg ((offset + c) `mod` nColor + 1)):otherPegs |
        otherPegs <- allStatesInternal nColor (nPeg-1) (offset+1),
        c <- [0..nColor-1]
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

-- | Accumulator for tallying feedback
accFeedback :: (Eq p)
    => p
    -> p
    -> ([p], [p], Feedback) -- ^ Unmatched guess pegs, unmatched true pegs, current feedback
    -> ([p], [p], Feedback)
accFeedback p q (leftPs, leftQs, feedback)
    | p == q
        = ( leftPs, leftQs, feedback <> Feedback 1 0 )
    | elem p leftQs && elem q leftPs
        = ( dropFirstMatch q leftPs, dropFirstMatch p leftQs, feedback <> Feedback 0 2 )
    | elem p leftQs
        = ( leftPs, q:(dropFirstMatch p leftQs), feedback <> Feedback 0 1 )
    | elem q leftPs
        = ( p:(dropFirstMatch q leftPs), leftQs, feedback <> Feedback 0 1 )
    | otherwise
        = ( p:leftPs, q:leftQs, feedback)

dropFirstMatch :: (Eq p) => p -> [p] -> [p]
dropFirstMatch p ps
    = let
        (beforeMatch, matchAndAfter) = span (/= p) ps
    in
        beforeMatch ++ drop 1 matchAndAfter

foldFeedback :: (Eq p)
    => [p]
    -> [p]
    -> ([p], [p], Feedback)
foldFeedback [] []  = ([], [], mempty)
foldFeedback _ []   = ([], [], InvalidFeedback)
foldFeedback [] _   = ([], [], InvalidFeedback)
foldFeedback (ph:pt) (qh:qt)
    = accFeedback ph qh $ foldFeedback pt qt

-- | Get the feedback for a peg guess
getFeedback
    :: [Peg]  -- ^ Guessed peg state
    -> [Peg]  -- ^ True peg state
    -> Feedback
getFeedback ps qs
    = case foldFeedback ps qs of
        (_, _, f) -> f
