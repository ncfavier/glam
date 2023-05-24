{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
import Data.Char
import Data.List
import Control.Monad
import Control.Monad.Loops
import Control.Monad.State
import System.Console.GetOpt
import System.Console.Haskeline
import System.Environment
import System.Exit
import System.IO

import Glam.Run

instance MonadState s m => MonadState s (InputT m) where
    get = lift get
    put = lift . put

err = liftIO . hPutStrLn stderr

usage = "usage: glam [options...] files..."

options = [Option ['i'] ["interactive"] (NoArg ()) "run in interactive mode (default if no files are provided)"]

parseArgs = do
    args <- getArgs
    (i, fs) <- case getOpt Permute options args of
        (o, fs, [])  -> pure (not (null o), fs)
        (_, _, errs) -> die $ concat errs ++ usageInfo usage options
    let interactive = i || null fs
    pure (interactive, fs)

comp = completeWord Nothing " \t" \p -> do
    defined <- getWords
    let words = defined ++ ["fst", "snd", "abort", "left", "right", "fold", "unfold", "box", "unbox", "next", "prev"]
    pure [simpleCompletion w | w <- words, p `isPrefixOf` w]

settings = Settings { complete = comp
                    , historyFile = Just ".glam_history"
                    , autoAddHistory = True }

prompt = "> "

greet = putStrLn "glam, the guarded λ-calculus (https://github.com/ncfavier/glam)"

main = runGlamT do
    (interactive, fs) <- liftIO parseArgs
    liftIO $ hSetBuffering stdout NoBuffering
    forM_ fs \f -> do
        let (name, contents) | f == "-"  = ("", getContents)
                             | otherwise = (f, readFile f)
        contents <- liftIO contents
        liftIO . either die (mapM_ putStrLn) =<< runFile name contents
    when interactive do
        liftIO greet
        runInputT settings repl

commands =
    [ "type" ==> \s -> do
        ty <- getType s
        liftIO case ty of
            Right ty -> putStrLn $ s ++ " : " ++ show ty
            Left e -> err e
    , "quit" ==> \_ -> liftIO exitSuccess
    ] where (==>) = (,)

repl = handleInterrupt repl $ withInterrupt $
    whileJust_ (getInputLine prompt) \(dropWhile isSpace -> line) -> case line of
        ':':(break isSpace -> (cmd, dropWhile isSpace -> args)) ->
            case [c | c@(name, _) <- commands, cmd `isPrefixOf` name] of
                [(_, action)] -> action args
                [] -> err $ "unknown command :" ++ cmd
                cs -> err $ "ambiguous command :" ++ cmd ++ " could refer to: " ++ intercalate " " (map fst cs)
        _ -> liftIO . either err (mapM_ putStrLn) =<< runFile "" line
