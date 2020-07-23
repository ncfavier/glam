{-# LANGUAGE TemplateHaskell #-}
module Glam.Inference where

import           Data.Traversable
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set
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
                             , _tvars :: [TVar] }

makeLenses ''InferState

initialInferState = InferState Map.empty tvars
    where tvars = ['\'':x | n <- [1..], x <- replicateM n ['a'..'z']]

type Environment = Map Var (Polytype, Bool)

type MonadInfer m = (MonadState InferState m, MonadReader Environment m, MonadError String m)

runInferT a env = runReaderT (evalStateT a initialInferState) env

expandTVar :: MonadInfer m => TVar -> m a -> (Type -> m a) -> m a
expandTVar x a b = maybe a b =<< use (unifier.at x)

freshTVar :: MonadInfer m => m Type
freshTVar = TVar . head <$> (tvars <<%= tail)
freshTVars n = replicateM n freshTVar

bindTVar :: MonadInfer m => TVar -> Type -> m ()
bindTVar x t | t == TVar x = return ()
             | x `freeInType` t = throwError $
                 "cannot construct infinite type " ++ x ++ " ~ " ++ show t
             | otherwise = unifier.at x ?= t

-- Return the canonical form of a type.
canonical :: MonadInfer m => Type -> m Type
canonical ty = do
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
    constantBinding (Forall _ ty, False) = do
        ty' <- canonical ty
        return (isClosed ty' && isConstant ty')

instantiate :: MonadInfer m => Polytype -> m Type
instantiate (Forall xs ty) = do
    ys <- freshTVars (length xs)
    return $ substituteType (Map.fromList (zip xs ys)) ty

generalise :: MonadInfer m => Type -> m Polytype
generalise ty = do
    ty' <- canonical ty
    free <- view (each._1.to freeTVars)
    return $ Forall (Set.toList (freeTVars ty' Set.\\ free)) ty'

-- Type unification
infix 5 !~
(!~) :: MonadInfer m => Type -> Type -> m ()
ty1          !~ ty2 | ty1 == ty2 = return ()
ta1 :*: tb1  !~ ta2 :*: tb2      = ta1 !~ ta2 >> tb1 !~ tb2
ta1 :+: tb1  !~ ta2 :+: tb2      = ta1 !~ ta2 >> tb1 !~ tb2
ta1 :->: tb1 !~ ta2 :->: tb2     = ta1 !~ ta2 >> tb1 !~ tb2
Later ty1    !~ Later ty2        = ty1 !~ ty2
Constant ty1 !~ Constant ty2     = ty1 !~ ty2
TFix x1 tf1  !~ TFix x2 tf2      = tf1 !~ substituteType1 x2 (TVar x1) tf2
TVar x       !~ ty               = expandTVar x (bindTVar x =<< canonical ty) (!~ ty)
ty           !~ TVar x           = TVar x !~ ty
ty1          !~ ty2              = throwError $ "cannot match " ++ show ty1 ++ " with " ++ show ty2

-- Type checking
infix 5 !:
(!:) :: MonadInfer m => Term -> Type -> m ()
Var x !: ty = view (at x) >>= \case
    Just (tyx, _) -> do
        tyx' <- instantiate tyx
        ty !~ tyx'
    Nothing -> throwError $ "unbound variable " ++ x
Int{} !: ty = ty !~ TInt
Plus a b !: ty = do
    a !: TInt
    b !: TInt
    ty !~ TInt
Minus a b !: ty = do
    a !: TInt
    b !: TInt
    ty !~ TInt
Unit !: ty = ty !~ One
Pair a b !: ty = do
    ~[ta, tb] <- freshTVars 2
    ty !~ ta :*: tb
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
    ty !~ ta :+: tb
    t !: ta
InR t !: ty = do
    ~[ta, tb] <- freshTVars 2
    ty !~ ta :+: tb
    t !: tb
Case t (Abs x1 t1) (Abs x2 t2) !: ty = do
    ~[ta, tb] <- freshTVars 2
    t !: ta :+: tb
    constant <- isConstantTerm t
    at x1 ?~ (Monotype ta, constant) |- t1 !: ty
    at x2 ?~ (Monotype tb, constant) |- t2 !: ty
Abs x t !: ty = do
    ~[ta, tb] <- freshTVars 2
    ty !~ ta :->: tb
    at x ?~ (Monotype ta, False) |- t !: tb
s :$: t !: ty = do
    ta <- freshTVar
    s !: ta :->: ty
    t !: ta
Let (Subst s t) !: ty = do
    e <- for s \t' -> do
        ty <- generalise =<< (t' ?:)
        constant <- isConstantTerm t'
        return (ty, constant)
    Map.union e |- t !: ty
Fold t !: ty = do
    ty <- canonical ty
    case ty of
        TFix x tf -> t !: substituteType1 x ty tf
        _ -> throwError $ "bad type for fold: " ++ show ty
Unfold t !: ty = do
    ty' <- canonical =<< (t ?:)
    case ty' of
        TFix x tf -> ty !~ substituteType1 x ty' tf
        _ -> throwError $ "bad type for unfold: " ++ show ty'
Fix (Abs x t) !: ty = at x ?~ (Monotype (Later ty), False) |- t !: ty
Next t !: ty = do
    ta <- freshTVar
    ty !~ Later ta
    t !: ta
Prev t !: ty = do
    t !: Later ty
    constant <- isConstantTerm t
    unless constant $ throwError $ "non-constant term for prev: " ++ show t
s :<*>: t !: ty = do
    ~[ta, tb] <- freshTVars 2
    ty !~ Later ta
    t !: Later tb
    s !: Later (tb :->: ta)
Box t !: ty = do
    ta <- freshTVar
    ty !~ Constant ta
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
