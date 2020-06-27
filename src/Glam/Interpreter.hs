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

type GlamState = Subst

type MonadGlam = MonadState GlamState

initialGlamState :: GlamState
initialGlamState = Map.empty

runGlam :: Monad m => StateT GlamState m a -> m a
runGlam a = evalStateT a initialGlamState

eval :: MonadGlam m => Term -> m Term
eval t = (`substitute` t) <$> get

getDefined :: MonadGlam m => m [Var]
getDefined = gets Map.keys

runFile :: MonadGlam m => String -> String -> m (Either String [String])
runFile name contents = runExceptT $ do
    cs <- liftEither $ runP file name contents
    execWriterT $ forM cs runStatement

runStatement :: (MonadGlam m, MonadWriter [String] m) => Statement -> m ()
runStatement (Define x t) = do
    t' <- eval t
    modify $ Map.insert x t'
runStatement (Print t) = do
    t' <- eval t
    tell [show (normalise t')]
