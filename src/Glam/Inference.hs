{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
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
import           Control.Lens hiding (Fold)

import Glam.Term
import Glam.Type

infix 1 |-
(|-) = local

instance {-# OVERLAPPABLE #-} MonadReader r m => MonadReader r (ReaderT r' m) where
    ask = lift ask
    local = mapReaderT . local

data InferState = InferState { _unifier :: Map TVar Type  -- The unifying substitution; maps unification variables to the type they are bound to
                             , _constantTVars :: Set TVar -- A set of unification variables that can only represent constant types
                             , _tvars :: [TVar] }         -- A list of fresh type variables

makeLenses ''InferState

type Environment = Map Var (Polytype, Bool)

type MonadInfer m = (MonadState InferState m, MonadReader Environment m, MonadError String m)

runInferT a xs env = runReaderT (evalStateT a initialInferState) env
    where initialInferState = InferState Map.empty Set.empty (freshTVarsFor xs)

expandTVar :: MonadInfer m => TVar -> m a -> (Type -> m a) -> m a
expandTVar x a b = maybe a b =<< use (unifier.at x)

freshTVar :: MonadInfer m => m Type
freshTVar = TVar . head <$> (tvars <<%= tail)
freshTVars n = replicateM n freshTVar

-- Return the canonical form of a type.
canonical ty = runReaderT (canonical' ty) Set.empty
canonicalPoly (Forall xs ty) = Forall xs <$> runReaderT (canonical' ty) (Set.fromList (map fst xs))

canonical' :: MonadInfer m => Type -> ReaderT (Set TVar) m Type
canonical' (TVar x)     = do
    s <- asks (Set.member x)
    if s then return (TVar x) else expandTVar x (return (TVar x)) canonical'
canonical' (a :*: b)    = (:*:) <$> canonical' a <*> canonical' b
canonical' (a :+: b)    = (:+:) <$> canonical' a <*> canonical' b
canonical' (a :->: b)   = (:->:) <$> canonical' a <*> canonical' b
canonical' (Later t)    = Later <$> canonical' t
canonical' (Constant t) = Constant <$> canonical' t
canonical' (TFix x t)   = TFix x <$> local (Set.insert x) (canonical' t)
canonical' ty           = return ty

-- Instantiate a polytype to a type by replacing polymorphic type variables with unification variables.
instantiate :: MonadInfer m => Polytype -> m Type
instantiate (Forall xs ty) = do
    ys <- freshTVars (length xs)
    constantTVars <>= Set.fromList [y | ((_, True), TVar y) <- zip xs ys]
    return $ substituteType (Map.fromList (zip (map fst xs) ys)) ty

-- Generalise a type to a polytype by closing on its variables that aren't free in the environment.
generalise :: MonadInfer m => Type -> m Polytype
generalise ty = do
    ty <- canonical ty
    freeInEnv <- fmap (foldMap freeTVars) . traverse canonicalPoly =<< views (each._1) (\x->[x])
    constantTVars <- use constantTVars
    let free = Set.toList (freeTVars ty Set.\\ freeInEnv)
    return $ Forall [(x, x `Set.member` constantTVars) | x <- free] ty

-- Test whether a type is constant, optionally forcing it by marking its type variables as constant.
constantType :: MonadInfer m => Bool -> Type -> m Bool
constantType force (TVar x)
    | force                    = constantTVars.contains x <.= True
    | otherwise                = use (constantTVars.contains x)
constantType force (t1 :*: t2) = liftA2 (&&) (constantType force t1) (constantType force t2)
constantType force (t1 :+: t2) = liftA2 (&&) (constantType force t1) (constantType force t2)
constantType force (_ :->: t2) = constantType force t2
constantType force ty@Later{}
    | force                    = throwError $ "non-constant type " ++ show ty
    | otherwise                = return False
constantType force (TFix _ t)  = constantType force t
constantType _ _               = return True

-- Test whether a term only depends on constant environment variables.
constantTerm :: MonadInfer m => Bool -> Term -> m Bool
constantTerm force t = do
    env <- ask
    and <$> traverse isConstantBinding (Map.restrictKeys env (freeVars t))
    where
    isConstantBinding (_, True) = return True
    isConstantBinding (ty, False) = constantType force =<< canonical =<< instantiate ty

bindTVar :: MonadInfer m => TVar -> Type -> m ()
bindTVar x ty
    | ty == TVar x = return ()
    | x `freeInType` ty = throwError $ "cannot construct infinite type " ++ x ++ " ~ " ++ show ty
    | otherwise = do
        constant <- use (constantTVars.contains x)
        when constant $ () <$ constantType True ty -- a constant unification variable can only be unified with a constant type
        unifier.at x ?= ty

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
    constant <- constantTerm False t
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
        constant <- constantTerm False t'
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
    () <$ constantTerm True t
s :<*>: t !: ty = do
    ~[ta, tb] <- freshTVars 2
    ty !~ Later ta
    t !: Later tb
    s !: Later (tb :->: ta)
Box t !: ty = do
    ta <- freshTVar
    ty !~ Constant ta
    t !: ta
    () <$ constantTerm True t
Unbox t !: ty = t !: Constant ty

-- Type inference
(?:) :: MonadInfer m => Term -> m Type
(?:) t = do
    ty <- freshTVar
    t !: ty
    return ty
