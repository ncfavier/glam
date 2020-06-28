{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
module Glam.Interpreter where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer

import Glam.Term
import Glam.Parse

data Statement = Define Var Term | Print Term
    deriving (Eq, Show)

type GlamState = Subst

type MonadGlam = MonadState GlamState

initialGlamState :: GlamState
initialGlamState = Map.empty

runGlamT :: Monad m => StateT GlamState m a -> m a
runGlamT a = evalStateT a initialGlamState

runGlam :: State GlamState a -> a
runGlam a = evalState a initialGlamState

eval :: MonadGlam m => Term -> m Term
eval t = (`substitute` t) <$> get

getDefined :: MonadGlam m => m [Var]
getDefined = gets Map.keys

statement :: Parser Statement
statement = uncurry Define <$> definition <|> Print <$> term

file :: Parser [Statement]
file = whitespace *> many (lineFolded statement) <* eof

runStatement :: (MonadGlam m, MonadWriter [String] m) => Statement -> m ()
runStatement (Define x t) = do
    t' <- eval t
    modify $ Map.insert x t'
runStatement (Print t) = do
    t' <- eval t
    tell [show (normalise t')]

runFile :: MonadGlam m => String -> String -> m (Either String [String])
runFile name contents = runExceptT $ do
    cs <- liftEither $ parse file name contents
    execWriterT $ forM cs runStatement