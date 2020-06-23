module Main where

import System.Console.Haskeline

import Glam.Term
import Glam.Parse

settings = defaultSettings { historyFile = Just ".glam" }

main = runInputT settings $ do
    outputStrLn "Welcome to glam."
    loop

loop = do
    input <- getInputLine "gλ> "
    case input of
        Nothing -> return ()
        Just input -> do
            case parseOne term input of
                Right t -> outputStrLn (show (normalise t))
                Left e -> outputStr e
            loop
