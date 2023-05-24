{-# LANGUAGE TemplateHaskell #-}
-- | The glam interpreter
module Glam.Run where

import Data.Functor.Identity
import Data.Map (Map)
import Data.Map qualified as Map
import Control.Applicative hiding (many, some)
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Lens
import Text.Megaparsec hiding (parse)

import Glam.Utils
import Glam.Term
import Glam.Type
import Glam.Rules.Term
import Glam.Rules.Type

-- | Program statements
data Statement = TypeDef TVar [TVar] Type -- ^ Type synonym definitions
               | Signature Var Polytype -- ^ Type signatures
               | Def Var Term -- ^ Definitions
               | Eval Term -- ^ Evaluate and print a term
               deriving (Eq, Show)

data GlamState = GlamState { _termBindings :: Map Var (Maybe Value, Polytype)
                           , _typeBindings :: Map TVar ([TVar], Type) }

makeLenses ''GlamState

type MonadGlam = MonadState GlamState

runGlamT :: Monad m => StateT GlamState m a -> m a
runGlamT a = evalStateT a (GlamState mempty mempty)

runGlam = runIdentity . runGlamT

evalTerm :: MonadGlam m => Term -> m Value
evalTerm t = do
    s <- Map.mapMaybe fst <$> use termBindings
    pure (eval s t)

withTypes a = do
    tys <- use typeBindings
    let tenv = Map.fromList [(x, Syn ys ty) | (x, (ys, ty)) <- Map.assocs tys]
    runReaderT a tenv

withTerms a = do
    ts <- use termBindings
    let env = Environment (Map.mapMaybe (\(t, ty) -> (ty, True) <$ t) ts) mempty
    runReaderT a env

getWords :: MonadGlam m => m [Var]
getWords = liftA2 (<>) (Map.keys <$> use termBindings)
                       (Map.keys <$> use typeBindings)

getType :: MonadGlam m => String -> m (Either String Polytype)
getType s = runExceptT do
    t <- liftEither $ parse term "" s
    withTerms $ inferTerm t

statement :: Parser Statement
statement = typeDef <|> signature <|> uncurry Def <$> binding <|> Eval <$> term
    where
    typeDef = TypeDef <$ "type" <*> tVar <*> many tVar <* equal <*> type_
    signature = try (Signature <$> variable <* colon) <*> polytype

file :: Parser [Statement]
file = whitespace *> many (lineFolded statement) <* eof

runFile :: MonadGlam m => String -> String -> m (Either String [String])
runFile name contents = runExceptT $ execWriterT do
    cs <- liftEither $ parse file name contents
    forM cs runStatement

runStatement :: (MonadGlam m, MonadWriter [String] m, MonadError String m) => Statement -> m ()
runStatement (TypeDef x ys ty) = do
    ty' <- withTypes $ checkTypeSynonym x ys ty
    typeBindings.at x ?= (ys, ty')
runStatement (Signature x ty) = do
    ty' <- withTypes $ checkPolytype ty
    termBindings.at x ?= (Nothing, ty')
runStatement (Def x t) = do
    ty <- use (termBindings.at x) >>= \case
        Just (_, ty) -> ty <$ withTerms (checkTerm t ty)
        Nothing -> withTerms $ inferTerm t
    t' <- evalTerm t
    termBindings.at x ?= (Just t', ty)
    tell [x ++ " : " ++ show ty]
runStatement (Eval t) = do
    ty <- withTerms $ inferTerm t
    t' <- evalTerm t
    tell [show t' ++ " : " ++ show (ty :: Polytype)]
