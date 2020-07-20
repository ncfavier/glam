module Glam.Interpreter where

import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.Except
import           Control.Monad.State
import           Control.Monad.Writer

import Glam.Term
import Glam.Type
import Glam.Parser
import Glam.Inference

data Statement = TypeDef TVar TypeConstructor
               | Signature Var Type
               | Binding Var Term
               | Eval Term
               deriving (Eq, Show)

type GlamState = Map Var (Maybe Term, Type)

type MonadGlam = MonadState GlamState

initialGlamState :: GlamState
initialGlamState = Map.empty

runGlamT :: Monad m => StateT GlamState m a -> m a
runGlamT a = evalStateT a initialGlamState

runGlam :: State GlamState a -> a
runGlam a = evalState a initialGlamState

eval :: MonadGlam m => Term -> m Term
eval t = do
    s <- Map.mapMaybe fst <$> get
    return (normalise (Let (Subst s t)))

getDefined :: MonadGlam m => m [Var]
getDefined = Map.keys <$> get

signature :: Parser (Var, Type)
signature = try ((,) <$> variable <* colon) <*> type_

statement :: Parser Statement
statement =  {-uncurry TypeDef   <$> typeDef
         <|> -}uncurry Signature <$> signature
         <|> uncurry Binding   <$> binding
         <|>         Eval      <$> term

file :: Parser [Statement]
file = whitespace *> many (lineFolded statement) <* eof

runFile :: MonadGlam m => String -> String -> m (Either String [String])
runFile name contents = runExceptT . execWriterT $ do
    cs <- liftEither $ parse file name contents
    forM cs runStatement

runStatement :: (MonadGlam m, MonadWriter [String] m, MonadError String m) => Statement -> m ()
runStatement (TypeDef _ _) = return ()
runStatement (Signature x ty) = do
    unless (isClosed ty) $ throwError $ "unbound type variables in " ++ show ty
    unless (isValid ty) $ throwError $ "invalid type " ++ show ty
    modify $ Map.insert x (Nothing, ty)
runStatement (Binding x t) = do
    binding <- Map.lookup x <$> get
    ty <- case binding of
        Just (_, ty) -> checkType ty t
        Nothing -> inferType t
    t' <- eval t
    modify $ Map.insert x (Just t', ty)
runStatement (Eval t) = do
    inferType t
    t' <- eval t
    tell [show t']

initialEnvironment :: MonadGlam m => m Environment
initialEnvironment = Map.mapMaybe (\(t, ty) -> (ty, True) <$ t) <$> get

checkType ty t = do
    runInferT (t ?: ty) =<< initialEnvironment
    return ty

inferType t = do
    ty <- runInferT (canon =<< (t ?:?)) =<< initialEnvironment
    unless (isClosed ty) $ throwError $ "ambiguous type " ++ show ty
    return ty
