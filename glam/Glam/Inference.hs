{-# LANGUAGE TemplateHaskell #-}
module Glam.Inference where

import Data.Maybe
import Data.Traversable
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Lens hiding (Fold)

import Glam.Term
import Glam.Type

infix 1 |-
(|-) = local

data InferState = InferState { _unifier :: Map TVar Type  -- The unifying substitution: maps metavariables to the type they are bound to
                                                          -- NOTE: there is no guarantee that this is well-scoped, but currently we don't
                                                          -- extend type contexts after metas are created.
                                                          -- This would change if we were to implement polytype signatures in let-bindings,
                                                          -- or higher-rank polymorphism, or existentials, etc.
                             , _constantTVars :: Set TVar -- Metavariables which can only represent constant types
                             , _tvars :: [TVar] }         -- A stream of fresh type variables

makeLenses ''InferState

data Environment = Environment { _termCtx :: Map Var (Polytype, Bool)
                               , _typeCtx :: Map TVar Bool }

makeLenses ''Environment

type MonadInfer m = (MonadState InferState m, MonadReader Environment m, MonadError String m)

runInferT a xs env = runReaderT (evalStateT a initialInferState) env
    where initialInferState = InferState Map.empty Set.empty (freshTVarsFor xs)

ifMeta :: MonadInfer m => Type -> (TVar -> m ()) -> m () -> m ()
ifMeta (TVar x) y n = maybe (y x) (const n) =<< view (typeCtx.at x)
ifMeta _ _ n = n

ifSolved :: MonadInfer m => TVar -> (Type -> m a) -> m a -> m a
ifSolved x y n = maybe n y =<< use (unifier.at x)

freshTVar :: MonadInfer m => m Type
freshTVar = TVar . head <$> (tvars <<%= tail)
freshTVars n = replicateM n freshTVar

-- Replace metavariables in a type with their solutions.
class Zonk t where
    zonk :: MonadInfer m => t -> m t
instance Zonk Type where
    zonk (TVar x)     = do
        s <- view (typeCtx.at x)
        if isJust s then return (TVar x) else ifSolved x zonk (return (TVar x))
    zonk (a :*: b)    = (:*:) <$> zonk a <*> zonk b
    zonk (a :+: b)    = (:+:) <$> zonk a <*> zonk b
    zonk (a :->: b)   = (:->:) <$> zonk a <*> zonk b
    zonk (Later t)    = Later <$> zonk t
    zonk (Constant t) = Constant <$> zonk t
    zonk (TFix x t)   = typeCtx.at x ?~ False |- TFix x <$> zonk t
    zonk ty           = return ty
instance Zonk Polytype where
    zonk (Forall xs ty) = typeCtx <>~ Map.fromList xs |- Forall xs <$> zonk ty

-- Instantiate a polytype to a type by replacing polymorphic type variables with fresh metavariables.
instantiate :: MonadInfer m => Polytype -> m Type
instantiate (Forall xs ty) = do
    ys <- freshTVars (length xs)
    constantTVars <>= Set.fromList [y | ((_, True), TVar y) <- zip xs ys]
    return $ substituteType (Map.fromList (zip (map fst xs) ys)) ty

-- Generalise a type to a polytype by closing on its variables that aren't free in the context.
generalise :: MonadInfer m => Type -> m Polytype
generalise ty = do
    ty <- zonk ty
    freeInCtx <- fmap (foldMap freeTVars) . traverse (zonk . fst) =<< view termCtx
    constantTVars <- use constantTVars
    let free = Set.toList (freeTVars ty Set.\\ freeInCtx)
    return $ Forall [(x, x `Set.member` constantTVars) | x <- free] ty

-- Test whether a type is constant, optionally forcing it to be by marking its type variables as constant.
constantType :: MonadInfer m => Bool -> Type -> m Bool
constantType force (TVar x) = do
    s <- view (typeCtx.at x)
    case s of
        Just False | force -> throwError $ "non-constant type variable " ++ x
        Just constant -> pure constant
        Nothing | force     -> constantTVars.contains x <.= True
                | otherwise -> use (constantTVars.contains x)
constantType force (t1 :*: t2) = (&&) <$> constantType force t1 <*> constantType force t2
constantType force (t1 :+: t2) = (&&) <$> constantType force t1 <*> constantType force t2
constantType force (_ :->: t2) = constantType force t2
constantType force ty@Later{}
    | force                    = throwError $ "non-constant type " ++ show ty
    | otherwise                = return False
constantType force (TFix _ t)  = constantType force t
constantType _     _           = return True

-- Test whether a term only depends on constant terms and types.
constantTerm :: MonadInfer m => Bool -> Term -> m Bool
constantTerm force t = do
    ctx <- view termCtx
    and <$> traverse isConstantBinding (Map.restrictKeys ctx (freeVars t))
    where
    isConstantBinding (_, True) = return True
    isConstantBinding (ty, False) = constantType force =<< zonk =<< instantiate ty

-- Unification

infix 5 !:=
(!:=) :: MonadInfer m => TVar -> Type -> m ()
x !:= ty = ifSolved x (!~ ty) (assign =<< zonk ty) where
    assign ty
        | ty == TVar x = pure ()
        | x `freeInType` ty = throwError $ "cannot construct infinite type " ++ x ++ " ~ " ++ show ty
        | otherwise = do
            constant <- use (constantTVars.contains x)
            when constant $ () <$ constantType True ty -- a constant type variable can only be unified with a constant type
            unifier.at x ?= ty

infix 4 !~
(!~) :: MonadInfer m => Type -> Type -> m ()
ta1 :*: tb1  !~ ta2 :*: tb2      = ta1 !~ ta2 >> tb1 !~ tb2
ta1 :+: tb1  !~ ta2 :+: tb2      = ta1 !~ ta2 >> tb1 !~ tb2
ta1 :->: tb1 !~ ta2 :->: tb2     = ta1 !~ ta2 >> tb1 !~ tb2
Later ty1    !~ Later ty2        = ty1 !~ ty2
Constant ty1 !~ Constant ty2     = ty1 !~ ty2
TFix x1 tf1  !~ TFix x2 tf2      = tf1 !~ substituteType1 x2 (TVar x1) tf2
ty1          !~ ty2 | ty1 == ty2 = return ()
                    | otherwise  = ifMeta ty1 (!:= ty2) $ ifMeta ty2 (!:= ty1) do
    ty1 <- zonk ty1
    ty2 <- zonk ty2
    throwError $ "cannot match type " ++ show ty1 ++ " with " ++ show ty2

-- Type checking

intrecType :: Polytype
intrecType = Forall [("a", False)] (("a" :->: "a") :->: "a" :->: ("a" :->: "a") :->: TInt :->: "a")

class CheckInfer t where
    infix 4 !:
    (!:) :: MonadInfer m => Term -> t -> m ()
    (?:) :: MonadInfer m => Term -> m t

instance CheckInfer Type where
    Var x !: ty = view (termCtx.at x) >>= \case
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
    Times a b !: ty = do
        a !: TInt
        b !: TInt
        ty !~ TInt
    Divide a b !: ty = do
        a !: TInt
        b !: TInt
        ty !~ TInt
    IntRec !: ty = (ty !~) =<< instantiate intrecType
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
    Case t ~(Abs x1 t1) ~(Abs x2 t2) !: ty = do
        ~[ta, tb] <- freshTVars 2
        t !: ta :+: tb
        constant <- constantTerm False t
        termCtx.at x1 ?~ (Monotype ta, constant) |- t1 !: ty
        termCtx.at x2 ?~ (Monotype tb, constant) |- t2 !: ty
    Abs x t !: ty = do
        ~[ta, tb] <- freshTVars 2
        ty !~ ta :->: tb
        termCtx.at x ?~ (Monotype ta, False) |- t !: tb
    s :$: t !: ty = do
        ta <- freshTVar
        s !: ta :->: ty
        t !: ta
    Let s t !: ty = do
        e <- for s \t' -> do
            ty <- (t' ?:)
            constant <- constantTerm False t'
            return (ty, constant)
        termCtx %~ Map.union e |- t !: ty
    Fold t !: ty = do
        ty <- zonk ty
        case ty of
            TFix x tf -> t !: substituteType1 x ty tf
            _ -> throwError $ "bad type for fold: " ++ show ty
    Unfold t !: ty = do
        ty' <- zonk =<< (t ?:)
        case ty' of
            TFix x tf -> ty !~ substituteType1 x ty' tf
            _ -> throwError $ "bad type for unfold: " ++ show ty'
    Fix ~(Abs x t) !: ty = termCtx.at x ?~ (Monotype (Later ty), False) |- t !: ty
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

    (?:) t = do
        ty <- freshTVar
        t !: ty
        return ty

instance CheckInfer Polytype where
    t !: Forall xs ty = typeCtx <>~ Map.fromList xs |- t !: ty

    (?:) = (?:) >=> generalise
