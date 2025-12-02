{-|
Module:         Text.IO
Description:    Helpers for text in terminal IO
 -}
module Text.IO (
    readUntilSucceed,
) where

import Text.Read (readEither)
import Data.Either (either)

readUntilSucceed
    :: (Read a)
    => String   -- ^ Main prompt
    -> IO a
readUntilSucceed mainPrompt = do
    putStrLn mainPrompt
    thisLine <- getLine
    either
        (\err -> do
            putStrLn $ "Problem reading your input: '" ++ err ++ "' Please try again!"
            readUntilSucceed mainPrompt
        )
        return
        (readEither thisLine)
