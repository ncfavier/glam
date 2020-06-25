{-# LANGUAGE CPP #-}
module Main where

import Glam.Term
import Glam.Parse

#ifdef __GHCJS__

import Data.JSString
import GHCJS.Marshal
import GHCJS.Foreign.Callback

foreign import javascript unsafe "init($1)"
    init_ :: Callback a -> IO ()

main = do
    eval <- syncCallback1' $ \v -> do
        Just input <- fromJSVal v
        toJSVal $ pack $ case parseOne term (unpack input) of
            Right t -> show (normalise t)
            Left e -> e
    init_ eval

#else

import System.Console.Haskeline

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

#endif
