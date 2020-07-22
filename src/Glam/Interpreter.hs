{-# LANGUAGE TemplateHaskell #-}
module Glam.Interpreter where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Lens.Micro.Platform

import Glam.Term
import Glam.Type
import Glam.Parser
import Glam.Inference

data Statement = TypeDef TVar [TVar] Type
               | Signature Var Type
               | Binding Var Term
               | Eval Term
               deriving (Eq, Show)

data GlamState = GlamState { _termBindings :: Map Var (Maybe Term, Type)
                           , _typeBindings :: Map TVar ([TVar], Type)
                           }

makeLenses ''GlamState

type MonadGlam = MonadState GlamState

initialGlamState :: GlamState
initialGlamState = GlamState Map.empty Map.empty

runGlamT :: Monad m => StateT GlamState m a -> m a
runGlamT a = evalStateT a initialGlamState

runGlam :: State GlamState a -> a
runGlam a = evalState a initialGlamState

eval :: MonadGlam m => Term -> m Term
eval t = do
    s <- Map.mapMaybe fst <$> use termBindings
    return (normalise (substitute s t))

evalType :: (MonadGlam m, MonadError String m) => Maybe (TVar, [TVar]) -> Type -> m Type
evalType self ty = do
    ty' <- runReaderT (desugar ty) env
    return $ if | Just (x, _) <- self, x `freeInType` ty' -> TFix x ty'
                | otherwise                               -> ty'
    where
    env | Just (_, args) <- self = Set.fromList args
        | otherwise              = Set.empty
    desugar ty@TVar{}     = apply ty []
    desugar ty@TApp{}     = apply ty []
    desugar (ta :*: tb)   = (:*:) <$> desugar ta <*> desugar tb
    desugar (ta :+: tb)   = (:+:) <$> desugar ta <*> desugar tb
    desugar (ta :->: tb)  = (:->:) <$> desugar ta <*> desugar tb
    desugar (Later ty)    = Later <$> desugar ty
    desugar (Constant ty) = Constant <$> desugar ty
    desugar (TFix x tf)   = TFix x <$> local (Set.insert x) (desugar tf)
    desugar ty            = return ty
    apply (TApp t1 t2) tys = do
        t2' <- desugar t2
        apply t1 (t2':tys)
    apply (TVar x) tys = ReaderT $ \env -> if
        | x `Set.member` env -> case tys of
            [] -> return (TVar x)
            _ -> throwError $ "not a type constructor: " ++ x
        | Just (y, args) <- self, x == y -> if
            | tys == map TVar args -> return (TVar x)
            | otherwise -> throwError $ "recursive type constructor " ++ x ++ " must be applied to the same arguments"
        | otherwise -> use (typeBindings.at x) >>= \case
            Just (args, ty)
                | length args == length tys -> return $ substituteType (Map.fromList (zip args tys)) ty
                | otherwise -> throwError $ "wrong number of arguments for type constructor " ++ x ++ ": got " ++ show (length tys) ++ ", expecting " ++ show (length args)
            Nothing -> throwError $ "unbound type constructor " ++ x
    apply ty _ = throwError $ "not a type constructor: " ++ show ty

getDefined :: MonadGlam m => m [Var]
getDefined = Map.keys <$> use termBindings

statement :: Parser Statement
statement = typeDef <|> signature <|> uncurry Binding <$> binding <|> Eval <$> term
    where
    typeDef = TypeDef <$ "type" <*> typeVariable <*> many typeVariable <* equal <*> type_
    signature = try (Signature <$> variable <* colon) <*> type_

file :: Parser [Statement]
file = whitespace *> many (lineFolded statement) <* eof

runFile :: MonadGlam m => String -> String -> m (Either String [String])
runFile name contents = runExceptT . execWriterT $ do
    cs <- liftEither $ parse file name contents
    forM cs runStatement

runStatement :: (MonadGlam m, MonadWriter [String] m, MonadError String m) => Statement -> m ()
runStatement (TypeDef x ys ty) = do
    ty' <- evalType (Just (x, ys)) ty
    typeBindings.at x ?= (ys, ty')
runStatement (Signature x ty) = do
    ty' <- evalType Nothing ty
    unless (isValid ty') $ throwError $ "invalid type " ++ show ty'
    termBindings.at x ?= (Nothing, ty')
runStatement (Binding x t) = do
    binding <- use (termBindings.at x)
    ty <- case binding of
        Just (_, ty) -> checkType ty t
        Nothing -> inferType t
    t' <- eval t
    termBindings.at x ?= (Just t', ty)
runStatement (Eval t) = do
    inferType t
    t' <- eval t
    tell [show t']

initialEnvironment :: MonadGlam m => m Environment
initialEnvironment = Map.mapMaybe (\(t, ty) -> (ty, True) <$ t) <$> use termBindings

checkType ty t = do
    runInferT (t !: ty) =<< initialEnvironment
    return ty

inferType t = do
    ty <- runInferT (canon =<< (t ?:)) =<< initialEnvironment
    unless (isClosed ty) $ throwError $ "ambiguous type " ++ show ty
    return ty
