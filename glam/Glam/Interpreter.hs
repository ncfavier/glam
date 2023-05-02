{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Glam.Interpreter where

import           Data.Functor.Identity
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Control.Applicative hiding (many, some)
import           Control.Monad
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

data GlamState = GlamState { _termBindings :: Map Var (Maybe Value, Polytype)
                           , _typeBindings :: Map TVar ([TVar], Type) }

makeLenses ''GlamState

type MonadGlam = MonadState GlamState

initialGlamState = GlamState Map.empty Map.empty

runGlamT :: Monad m => StateT GlamState m a -> m a
runGlamT a = evalStateT a initialGlamState

runGlam = runIdentity . runGlamT

evalTerm :: MonadGlam m => Term -> m Value
evalTerm t = do
    s <- Map.mapMaybe fst <$> use termBindings
    return (eval s t)

data Guardedness = Unguarded | Guarded | Forbidden
data TVarBinding = Syn [TVar] Type | Self Guardedness (Maybe [TVar]) | Argument

evalType :: (MonadGlam m, MonadError String m) => Maybe (TVar, [TVar]) -> Polytype -> m Polytype
evalType self (Forall as ty) = do
    tbs <- use typeBindings
    let env = Map.fromList $ [(x, Syn ys ty) | (x, (ys, ty)) <- Map.assocs tbs] ++
                             [(x, Self Unguarded (Just ys)) | Just (x, ys) <- [self]] ++
                             [(x, Argument) | x <- maybe [] snd self ++ map fst as]
    ty' <- runReaderT (go ty) env
    return $ Forall as (autoTFix ty')
    where
    later (Self Unguarded as) = Self Guarded as
    later v = v
    constant (Self _ as) = Self Forbidden as
    constant v = v
    go ty@TVar{}     = apply ty []
    go ty@TApp{}     = apply ty []
    go (ta :*: tb)   = (:*:) <$> go ta <*> go tb
    go (ta :+: tb)   = (:+:) <$> go ta <*> go tb
    go (ta :->: tb)  = (:->:) <$> go ta <*> go tb
    go (Later ty)    = Later <$> local (fmap later) (go ty)
    go (Constant ty) = Constant <$> local (fmap constant) (go ty)
    go (TFix x tf)   = TFix x <$> local (at x ?~ Self Unguarded Nothing) (go tf)
    go ty            = return ty
    apply (TApp t1 t2) tys = do
        t2' <- go t2
        apply t1 (t2':tys)
    apply (TVar x) tys = view (at x) >>= maybe (throwError $ "unbound type variable " ++ x) \case
        Syn ys ty
            | length ys == length tys -> return $ substituteType (Map.fromList (zip ys tys)) ty
            | otherwise -> throwError $
                "wrong number of arguments for type constructor " ++ x ++ ": got " ++
                show (length tys) ++ ", expecting " ++ show (length ys)
        Self _ (Just ys) | tys /= map TVar ys -> throwError $
            "recursive type constructor " ++ x ++ " must be applied to the same arguments"
        Self Unguarded _ -> throwError $ "unguarded fixed point variable " ++ x
        Self Forbidden _ -> throwError $ "fixed point variable " ++ x ++ " cannot appear under #"
        Self Guarded _ -> return (TVar x)
        _ | null tys -> return (TVar x)
          | otherwise -> throwError $ "not a type constructor: " ++ x
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
    termBindings.at x ?= (Nothing, ty')
runStatement (Def x t) = do
    ty <- use (termBindings.at x) >>= \case
        Just (_, ty) -> ty <$ checkType ty t
        Nothing -> inferType t
    t' <- evalTerm t
    termBindings.at x ?= (Just t', ty)
    tell [x ++ " : " ++ show ty]
runStatement (Eval t) = do
    ty <- inferType t
    t' <- evalTerm t
    tell [show t' ++ " : " ++ show ty]

initialEnvironment :: MonadGlam m => m Environment
initialEnvironment = Map.mapMaybe (\(t, ty) -> (ty, True) <$ t) <$> use termBindings

checkType ty t = runInferT (t !: ty)               (allTVars ty) =<< initialEnvironment
inferType    t = runInferT ((t ?:) >>= generalise) Set.empty     =<< initialEnvironment
