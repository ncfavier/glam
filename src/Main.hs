{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.List (isPrefixOf)
import Control.Monad.Loops
import Control.Monad.State
import System.Console.GetOpt
import System.Console.Haskeline
import System.Environment
import System.Exit
import System.IO

import Glam.Interpreter

instance MonadState s m => MonadState s (InputT m) where
    get = lift get
    put = lift . put

die' s = hPutStr stderr s >> exitFailure

usage = "usage: glam [options...] files..."

options = [Option ['i'] ["interactive"] (NoArg ()) "run in interactive mode"]

parseArgs = do
    args <- getArgs
    (i, fs) <- case getOpt Permute options args of
        (o, fs, [])  -> pure (not (null o), fs)
        (_, _, errs) -> die' $ concat errs ++ usageInfo usage options
    let interactive = i || null fs
    return (interactive, fs)

comp = completeWord Nothing " \t" $ \p -> do
    defined <- getDefined
    let words = defined ++ ["fst", "snd", "left", "right", "fold", "unfold", "box", "unbox", "next", "prev"]
    return [simpleCompletion w | w <- words, p `isPrefixOf` w]

settings = Settings { complete = comp
                    , historyFile = Just ".glam_history"
                    , autoAddHistory = True }

prompt = "> "

greet = putStrLn "glam, the guarded λ-calculus (https://git.monade.li/glam)"

main = runGlamT $ do
    (interactive, fs) <- liftIO parseArgs
    forM_ fs $ \f -> do
        let (name, contents) | f == "-"  = ("", getContents)
                             | otherwise = (f, readFile f)
        contents <- liftIO contents
        liftIO . either die' (mapM_ putStrLn) =<< runFile name contents
    when interactive $ do
        liftIO greet
        runInputT settings repl

repl = handleInterrupt repl $ withInterrupt $
    whileJust_ (getInputLine prompt) $ \line ->
        liftIO . either (hPutStr stderr) (mapM_ putStrLn) =<< runFile "" line
