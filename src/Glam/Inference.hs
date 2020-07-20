module Glam.Inference where

import           Data.Traversable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State

import Glam.Term
import Glam.Type

data InferState = InferState { typeBindings :: Map TVar Type, tvars :: [TVar] }

initialInferState = InferState Map.empty tvars
    where tvars = map ('\'':) $ [1..] >>= (`replicateM` ['a'..'z'])

type Environment = Map Var (Type, Bool)

type MonadInfer m = (MonadState InferState m, MonadReader Environment m, MonadError String m)

runInferT a env = runReaderT (evalStateT a initialInferState) env

expandTVar :: MonadInfer m => TVar -> m a -> (Type -> m a) -> m a
expandTVar x a b = maybe a b . Map.lookup x =<< gets typeBindings

freshTVar :: MonadInfer m => m Type
freshTVar = do
    ~(x:tvars) <- gets tvars
    modify $ \s -> s { tvars }
    return (TVar x)
freshTVars n = replicateM n freshTVar

bindTVar :: MonadInfer m => TVar -> Type -> m ()
bindTVar x t | x `freeInType` t = throwError $
                 "cannot construct infinite type " ++ x ++ " ~ " ++ show t
             | otherwise = modify $ \s -> s { typeBindings = Map.insert x t (typeBindings s) }

-- Return the canonical form of a type.
canon :: MonadInfer m => Type -> m Type
canon (TVar x)     = expandTVar x (return (TVar x)) canon
canon (a :*: b)    = (:*:) <$> canon a <*> canon b
canon (a :+: b)    = (:+:) <$> canon a <*> canon b
canon (a :->: b)   = (:->:) <$> canon a <*> canon b
canon (Later t)    = Later <$> canon t
canon (Constant t) = Constant <$> canon t
canon (TFix x t)   = TFix x <$> canon t
canon ty           = return ty

-- Test whether a term only depends on constant environment variables.
isConstantTerm :: MonadInfer m => Term -> m Bool
isConstantTerm t = do
    env <- ask
    and <$> traverse constantBinding (Map.restrictKeys env (freeVariables t))
    where
    constantBinding (_, True) = return True
    constantBinding (ty, False) = liftA2 (&&) isClosed isConstant <$> canon ty

-- Unify two types.
infix 5 ?=
(?=) :: MonadInfer m => Type -> Type -> m ()
t1          ?= t2 | t1 == t2 = return ()
a1 :*: b1   ?= a2 :*: b2     = a1 ?= a2 >> b1 ?= b2
a1 :+: b1   ?= a2 :+: b2     = a1 ?= a2 >> b1 ?= b2
a1 :->: b1  ?= a2 :->: b2    = a1 ?= a2 >> b1 ?= b2
Later t1    ?= Later t2      = t1 ?= t2
Constant t1 ?= Constant t2   = t1 ?= t2
TFix x1 t1  ?= TFix x2 t2    = t1 ?= substituteType x2 (TVar x1) t2
TVar x      ?= t             = expandTVar x (bindTVar x =<< canon t) (?= t)
t           ?= TVar x        = TVar x ?= t
t1          ?= t2            = throwError $ "cannot match " ++ show t1 ++ " with " ++ show t2

-- Type checking
infix 5 ?:
(?:) :: MonadInfer m => Term -> Type -> m ()
Var x ?: ty = do
    m <- Map.lookup x <$> ask
    case m of
        Just (tyx, _) -> ty ?= tyx
        Nothing -> throwError $ "unbound variable " ++ x
Int{} ?: ty = ty ?= TInt
Plus a b ?: ty = do
    a ?: TInt
    b ?: TInt
    ty ?= TInt
Minus a b ?: ty = do
    a ?: TInt
    b ?: TInt
    ty ?= TInt
Unit ?: ty = ty ?= One
Pair a b ?: ty = do
    ~[ta, tb] <- freshTVars 2
    ty ?= ta :*: tb
    a ?: ta
    b ?: tb
Fst t ?: ty = do
    tb <- freshTVar
    t ?: ty :*: tb
Snd t ?: ty = do
    ta <- freshTVar
    t ?: ta :*: ty
Abort t ?: _ = t ?: Zero
InL t ?: ty = do
    ~[ta, tb] <- freshTVars 2
    ty ?= ta :+: tb
    t ?: ta
InR t ?: ty = do
    ~[ta, tb] <- freshTVars 2
    ty ?= ta :+: tb
    t ?: tb
Case t (Abs x1 t1) (Abs x2 t2) ?: ty = do
    ~[ta, tb] <- freshTVars 2
    t ?: ta :+: tb
    constant <- isConstantTerm t
    local (Map.insert x1 (ta, constant)) (t1 ?: ty)
    local (Map.insert x2 (tb, constant)) (t2 ?: ty)
Abs x t ?: ty = do
    ~[ta, tb] <- freshTVars 2
    ty ?= ta :->: tb
    local (Map.insert x (ta, False)) (t ?: tb)
s :$: t ?: ty = do
    ta <- freshTVar
    s ?: ta :->: ty
    t ?: ta
Let (Subst s t) ?: ty = do
    e <- for s (liftA2 (liftA2 (,)) (?:?) isConstantTerm)
    local (Map.union e) (t ?: ty)
Fold t ?: ty = do
    ty <- canon ty
    case ty of
        TFix x tf | isClosed ty -> t ?: substituteType x ty tf
        _ -> throwError $ "bad type for fold: " ++ show ty
Unfold t ?: ty = do
    ty' <- canon =<< (t ?:?)
    case ty' of
        TFix x tf | isClosed ty' -> ty ?= substituteType x ty' tf
        _ -> throwError $ "bad type for unfold: " ++ show ty'
Fix (Abs x t) ?: ty = local (Map.insert x (Later ty, False)) (t ?: ty)
Next t ?: ty = do
    ta <- freshTVar
    ty ?= Later ta
    t ?: ta
Prev t ?: ty = do
    t ?: Later ty
    constant <- isConstantTerm t
    unless constant $ throwError $ "non-constant term for prev: " ++ show t
s :<*>: t ?: ty = do
    ~[ta, tb] <- freshTVars 2
    ty ?= Later ta
    t ?: Later tb
    s ?: Later (tb :->: ta)
Box t ?: ty = do
    ta <- freshTVar
    ty ?= Constant ta
    t ?: ta
    constant <- isConstantTerm t
    unless constant $ throwError $ "non-constant term for box: " ++ show t
Unbox t ?: ty = t ?: Constant ty

-- Type inference
(?:?) :: MonadInfer m => Term -> m Type
(?:?) t = do
    ty <- freshTVar
    t ?: ty
    return ty