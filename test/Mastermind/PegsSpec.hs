module Mastermind.PegsSpec (spec) where

import Test.Hspec
import Mastermind.Pegs

spec :: Spec
spec = do
  describe "Mastermind.Pegs" $ do
    describe "allStates" $ do
      it "generates the correct number of states (2 colors, 2 slots)" $ do
        length (allStates 2 2) `shouldBe` 4

      it "generates the correct number of states (3 colors, 3 slots)" $ do
        length (allStates 3 3) `shouldBe` 27

      it "includes expected combinations" $ do
        let states = allStates 2 2
        states `shouldContain` [[Peg 1, Peg 1]]
        states `shouldContain` [[Peg 1, Peg 2]]
        states `shouldContain` [[Peg 2, Peg 1]]
        states `shouldContain` [[Peg 2, Peg 2]]

    describe "getFeedback" $ do
      it "returns all reds for an exact match" $ do
        getFeedback [Peg 1, Peg 2] [Peg 1, Peg 2] `shouldBe` Feedback 2 0

      it "returns all whites for a completely swapped guess" $ do
        getFeedback [Peg 1, Peg 2] [Peg 2, Peg 1] `shouldBe` Feedback 0 2

      it "returns all blacks for a complete miss" $ do
        getFeedback [Peg 1, Peg 2] [Peg 3, Peg 4] `shouldBe` Feedback 0 0

      it "returns one white for one overlap" $ do
        getFeedback [Peg 1, Peg 2] [Peg 2, Peg 4] `shouldBe` Feedback 0 1

      it "returns one red for one overlap in the same place" $ do
        getFeedback [Peg 1, Peg 2] [Peg 1, Peg 4] `shouldBe` Feedback 1 0

      it "returns mixed feedback correctly" $ do
        -- 1 matches 1 (Red)
        -- 2 matches 2 (White, wrong pos)
        -- 3 matches 3 (White, wrong pos)
        getFeedback [Peg 1, Peg 2, Peg 3] [Peg 1, Peg 3, Peg 2] `shouldBe` Feedback 1 2

      it "handles duplicates in guess correctly" $ do
        -- Guess: 1, 1. Truth: 1, 2.
        -- First 1 is Red. Second 1 is unused.
        getFeedback [Peg 1, Peg 1] [Peg 1, Peg 2] `shouldBe` Feedback 1 0

      it "handles duplicates in truth correctly" $ do
        -- Guess: 1, 2. Truth: 1, 1.
        -- 1 is Red. 2 is unused.
        getFeedback [Peg 1, Peg 2] [Peg 1, Peg 1] `shouldBe` Feedback 1 0

      it "returns InvalidFeedback on length mismatch" $ do
        getFeedback [Peg 1] [Peg 1, Peg 2] `shouldBe` InvalidFeedback
