{-# LANGUAGE TemplateHaskell #-}
module Glam.Interpreter where

import           Data.Functor.Identity
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Applicative hiding (many, some)
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Control.Monad.Writer
import           Control.Lens

import Glam.Term
import Glam.Type
import Glam.Parser
import Glam.Inference

data Statement = TypeDef TVar [TVar] Type
               | Signature Var Polytype
               | Def Var Term
               | Eval Term
               deriving (Eq, Show)

data GlamState = GlamState { _termBindings :: Map Var (Maybe Term, Polytype)
                           , _typeBindings :: Map TVar ([TVar], Type) }

makeLenses ''GlamState

type MonadGlam = MonadState GlamState

initialGlamState = GlamState Map.empty Map.empty

runGlamT :: Monad m => StateT GlamState m a -> m a
runGlamT a = evalStateT a initialGlamState

runGlam = runIdentity . runGlamT

eval :: MonadGlam m => Term -> m Term
eval t = do
    s <- Map.mapMaybe fst <$> use termBindings
    return (normalise (substitute s t))

evalType :: (MonadGlam m, MonadError String m) => Maybe (TVar, [TVar]) -> Polytype -> m Polytype
evalType self (Forall as ty) = do
    ty' <- runReaderT (desugar ty) env
    return $ Forall as (autoTFix ty')
    where
    env = Set.fromList (maybe [] snd self ++ map fst as)
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
    apply (TVar x) tys = ask >>= \env -> if
        | x `Set.member` env -> case tys of
            [] -> return (TVar x)
            _ -> throwError $ "not a type constructor: " ++ x
        | Just (y, args) <- self, x == y -> if
            | tys == map TVar args -> return (TVar x)
            | otherwise -> throwError $
                "recursive type constructor " ++ x ++ " must be applied to the same arguments"
        | otherwise -> use (typeBindings.at x) >>= \case
            Just (args, ty)
                | length args == length tys -> return $ substituteType (Map.fromList (zip args tys)) ty
                | otherwise -> throwError $
                    "wrong number of arguments for type constructor " ++ x ++ ": got " ++
                    show (length tys) ++ ", expecting " ++ show (length args)
            Nothing -> throwError $ "unbound type constructor " ++ x
    apply ty _ = throwError $ "not a type constructor: " ++ show ty
    autoTFix ty | Just (x, _) <- self, x `freeInType` ty = TFix x ty
                | otherwise                              = ty

getWords :: MonadGlam m => m [Var]
getWords = liftA2 (<>) (Map.keys <$> use termBindings)
                       (Map.keys <$> use typeBindings)

getType s = runExceptT do
    t <- liftEither $ parse term "" s
    inferType t

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
    ~(Monotype ty') <- evalType (Just (x, ys)) (Monotype ty)
    typeBindings.at x ?= (ys, ty')
runStatement (Signature x ty) = do
    ty' <- evalType Nothing ty
    unless (isValid ty') $ throwError $ "invalid type " ++ show ty'
    termBindings.at x ?= (Nothing, ty')
runStatement (Def x t) = do
    ty <- use (termBindings.at x) >>= \case
        Just (_, ty) -> ty <$ checkType ty t
        Nothing -> inferType t
    t' <- eval t
    termBindings.at x ?= (Just t', ty)
    tell [x ++ " : " ++ show ty]
runStatement (Eval t) = do
    ty <- inferType t
    t' <- eval t
    tell [show t' ++ " : " ++ show ty]

initialEnvironment :: MonadGlam m => m Environment
initialEnvironment = Map.mapMaybe (\(t, ty) -> (ty, True) <$ t) <$> use termBindings

checkType ty t = runInferT ((t !:) =<< instantiate ty) =<< initialEnvironment

inferType t = runInferT (generalise =<< (t ?:)) =<< initialEnvironment
