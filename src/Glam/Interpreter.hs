{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Glam.Interpreter where

import           Data.Functor.Identity
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
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

evalTerm :: MonadGlam m => Term -> m Term
evalTerm t = do
    s <- Map.mapMaybe fst <$> use termBindings
    return (normalise (substitute s t))

data FixedStatus = Unguarded | Guarded | Forbidden
data TVarBinding = Fixed FixedStatus (Maybe [TVar]) | Syn [TVar] Type | Ok

evalType :: (MonadGlam m, MonadError String m) => Maybe (TVar, [TVar]) -> Polytype -> m Polytype
evalType self (Forall as ty) = do
    tbs <- use typeBindings
    let env = Map.fromList $ [(x, Syn ys ty) | (x, (ys, ty)) <- Map.assocs tbs] ++
                             [(x, Fixed Unguarded (Just ys)) | Just (x, ys) <- [self]] ++
                             [(x, Ok) | x <- maybe [] snd self ++ map fst as]
    ty' <- runReaderT (go ty) env
    return $ Forall as (autoTFix ty')
    where
    later (Fixed Unguarded as) = Fixed Guarded as
    later v = v
    constant (Fixed _ as) = Fixed Forbidden as
    constant v = v
    go ty@TVar{}     = apply ty []
    go ty@TApp{}     = apply ty []
    go (ta :*: tb)   = (:*:) <$> go ta <*> go tb
    go (ta :+: tb)   = (:+:) <$> go ta <*> go tb
    go (ta :->: tb)  = (:->:) <$> go ta <*> go tb
    go (Later ty)    = Later <$> local (fmap later) (go ty)
    go (Constant ty) = Constant <$> local (fmap constant) (go ty)
    go (TFix x tf)   = TFix x <$> local (at x ?~ Fixed Unguarded Nothing) (go tf)
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
        Fixed _ (Just ys) | tys /= map TVar ys -> throwError $
            "recursive type constructor " ++ x ++ " must be applied to the same arguments"
        Fixed Unguarded _ -> throwError $ "unguarded fixed point variable " ++ x
        Fixed Forbidden _ -> throwError $ "fixed point variable " ++ x ++ " cannot appear under #"
        Fixed Guarded _ -> return (TVar x)
        _ | not (null tys) -> throwError $ "not a type constructor: " ++ x
        _ -> return (TVar x)
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

checkType ty t = runInferT ((t !:) =<< instantiate ty) (allTVars ty) =<< initialEnvironment
inferType    t = runInferT ((t ?:) >>= generalise)     Set.empty     =<< initialEnvironment
