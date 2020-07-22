{-# LANGUAGE TemplateHaskell #-}
module Glam.Inference where

import           Data.Traversable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Applicative
import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.State
import           Lens.Micro.Platform

import Glam.Term
import Glam.Type

infix 1 |-
(|-) = local

data InferState = InferState { _unifier :: Map TVar Type
                             , _tvars :: [TVar]
                             }

makeLenses ''InferState

initialInferState = InferState Map.empty tvars
    where tvars = ['\'':x | n <- [1..], x <- replicateM n ['a'..'z']]

type Environment = Map Var (Type, Bool)

type MonadInfer m = (MonadState InferState m, MonadReader Environment m, MonadError String m)

runInferT a env = runReaderT (evalStateT a initialInferState) env

expandTVar :: MonadInfer m => TVar -> m a -> (Type -> m a) -> m a
expandTVar x a b = maybe a b =<< use (unifier.at x)

freshTVar :: MonadInfer m => m Type
freshTVar = TVar . head <$> (tvars <<%= tail)
freshTVars n = replicateM n freshTVar

bindTVar :: MonadInfer m => TVar -> Type -> m ()
bindTVar x t | x `freeInType` t = throwError $
                 "cannot construct infinite type " ++ x ++ " ~ " ++ show t
             | otherwise = unifier.at x ?= t

-- Return the canonical form of a type.
canon :: MonadInfer m => Type -> m Type
canon ty = do
    unifier <- use unifier
    let go (TVar x)      = maybe (TVar x) go $ Map.lookup x unifier
        go (ta :*: tb)   = go ta :*: go tb
        go (ta :+: tb)   = go ta :+: go tb
        go (ta :->: tb)  = go ta :->: go tb
        go (Later ty)    = Later (go ty)
        go (Constant ty) = Constant (go ty)
        go (TFix x tf)   = TFix x (go tf)
        go ty            = ty
    return (go ty)

-- Test whether a term only depends on constant environment variables.
isConstantTerm :: MonadInfer m => Term -> m Bool
isConstantTerm t = do
    env <- ask
    and <$> traverse constantBinding (Map.restrictKeys env (freeVars t))
    where
    constantBinding (_, True) = return True
    constantBinding (ty, False) = liftA2 (&&) isClosed isConstant <$> canon ty

-- Type unification
infix 5 !=
(!=) :: MonadInfer m => Type -> Type -> m ()
t1          != t2 | t1 == t2 = return ()
a1 :*: b1   != a2 :*: b2     = a1 != a2 >> b1 != b2
a1 :+: b1   != a2 :+: b2     = a1 != a2 >> b1 != b2
a1 :->: b1  != a2 :->: b2    = a1 != a2 >> b1 != b2
Later t1    != Later t2      = t1 != t2
Constant t1 != Constant t2   = t1 != t2
TFix x1 t1  != TFix x2 t2    = t1 != substituteType1 x2 (TVar x1) t2
TVar x      != t             = expandTVar x (bindTVar x =<< canon t) (!= t)
t           != TVar x        = TVar x != t
t1          != t2            = throwError $ "cannot match " ++ show t1 ++ " with " ++ show t2

-- Type checking
infix 5 !:
(!:) :: MonadInfer m => Term -> Type -> m ()
Var x !: ty = view (at x) >>= \case
    Just (tyx, _) -> ty != tyx
    Nothing -> throwError $ "unbound variable " ++ x
Int{} !: ty = ty != TInt
Plus a b !: ty = do
    a !: TInt
    b !: TInt
    ty != TInt
Minus a b !: ty = do
    a !: TInt
    b !: TInt
    ty != TInt
Unit !: ty = ty != One
Pair a b !: ty = do
    ~[ta, tb] <- freshTVars 2
    ty != ta :*: tb
    a !: ta
    b !: tb
Fst t !: ty = do
    tb <- freshTVar
    t !: ty :*: tb
Snd t !: ty = do
    ta <- freshTVar
    t !: ta :*: ty
Abort t !: _ = t !: Zero
InL t !: ty = do
    ~[ta, tb] <- freshTVars 2
    ty != ta :+: tb
    t !: ta
InR t !: ty = do
    ~[ta, tb] <- freshTVars 2
    ty != ta :+: tb
    t !: tb
Case t (Abs x1 t1) (Abs x2 t2) !: ty = do
    ~[ta, tb] <- freshTVars 2
    t !: ta :+: tb
    constant <- isConstantTerm t
    at x1 ?~ (ta, constant) |- t1 !: ty
    at x2 ?~ (tb, constant) |- t2 !: ty
Abs x t !: ty = do
    ~[ta, tb] <- freshTVars 2
    ty != ta :->: tb
    at x ?~ (ta, False) |- t !: tb
s :$: t !: ty = do
    ta <- freshTVar
    s !: ta :->: ty
    t !: ta
Let (Subst s t) !: ty = do
    e <- for s (liftA2 (liftA2 (,)) (?:) isConstantTerm)
    Map.union e |- t !: ty
Fold t !: ty = do
    ty <- canon ty
    case ty of
        TFix x tf | isClosed ty -> t !: substituteType1 x ty tf
        _ -> throwError $ "bad type for fold: " ++ show ty
Unfold t !: ty = do
    ty' <- canon =<< (t ?:)
    case ty' of
        TFix x tf | isClosed ty' -> ty != substituteType1 x ty' tf
        _ -> throwError $ "bad type for unfold: " ++ show ty'
Fix (Abs x t) !: ty = at x ?~ (Later ty, False) |- t !: ty
Next t !: ty = do
    ta <- freshTVar
    ty != Later ta
    t !: ta
Prev t !: ty = do
    t !: Later ty
    constant <- isConstantTerm t
    unless constant $ throwError $ "non-constant term for prev: " ++ show t
s :<*>: t !: ty = do
    ~[ta, tb] <- freshTVars 2
    ty != Later ta
    t !: Later tb
    s !: Later (tb :->: ta)
Box t !: ty = do
    ta <- freshTVar
    ty != Constant ta
    t !: ta
    constant <- isConstantTerm t
    unless constant $ throwError $ "non-constant term for box: " ++ show t
Unbox t !: ty = t !: Constant ty

-- Type inference
(?:) :: MonadInfer m => Term -> m Type
(?:) t = do
    ty <- freshTVar
    t !: ty
    return ty
