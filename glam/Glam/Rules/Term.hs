{-# LANGUAGE TemplateHaskell #-}
-- | The rules for type checking and type inference.
module Glam.Rules.Term where

import Data.Traversable
import Data.Maybe
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Control.Monad
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Control.Lens hiding (Fold)
import Text.Megaparsec

import Glam.Utils
import Glam.Term
import Glam.Type

data Environment = Environment
    { _termCtx :: Map Var (Polytype, Constancy) -- ^ The term context
    , _typeCtx :: [(TVar, Constancy)] -- ^ The type context
    }

makeLenses ''Environment

-- | A metavariable: a placeholder for a type to be determined later (or generalised over).
data Meta = Meta
    { _solution :: Maybe Type -- ^ The metavariable's solution
    , _constant :: Constancy
    , _level :: Int -- ^ The de Bruijn level at which this metavariable was created.
                    -- Solutions may not contain variables introduced later, as they would escape their scope.
                    -- An invariant is that (currently) fixed point variables are never in scope of a metavariable, so we
                    -- don't have to worry about substitution.
    }

makeLenses ''Meta

data UnificationState = UnificationState
    { _metas :: Map TVar Meta -- ^ Metavariables
    , _tvars :: [TVar] -- ^ A stream of fresh type variables
    }

makeLenses ''UnificationState

type MonadCheckTerm m = (MonadState UnificationState m, MonadReader Environment m, MonadError String m)

runUnification a xs = evalStateT a $ UnificationState mempty (freshTVarsFor xs)

-- | Case split on whether a type variable is bound or is a metavariable.
-- Note that since we use the same constructor for both, conflicts are possible.
-- In that case, we prefer bound type variables. This is fragile and probably buggy.
viewTVar :: MonadCheckTerm m => TVar -> m (Either Meta Constancy)
viewTVar x = maybe (throwError $ "unbound type variable " ++ x) pure =<< runMaybeT (
    Right <$> MaybeT (lookup x <$> view typeCtx) <|> Left <$> MaybeT (use (metas.at x)))

ifMeta :: MonadCheckTerm m => Type -> ((TVar, Meta) -> m ()) -> m () -> m ()
ifMeta (TVar x) y n = maybe n (curry y x) =<< use (metas.at x)
ifMeta _ _ n = n

freshTVar :: MonadCheckTerm m => m TVar
freshTVar = tvars %%= fromJust . uncons

-- | Create a new metavariable.
newMeta' :: MonadCheckTerm m => Constancy -> m Type
newMeta' c = do
    x <- freshTVar
    l <- length <$> view typeCtx
    metas.at x ?= Meta Nothing c l
    pure (TVar x)
-- | Create a new not-necessarily-constant metavariable.
newMeta = newMeta' False
newMetas n = replicateM n newMeta

class Zonk t where
    -- | Recursively replace solved metavariables in a type with their solutions.
    zonk :: MonadCheckTerm m => t -> m t

instance Zonk Type where
    zonk ty@(TVar x)  = viewTVar x >>= \case
        Left m | Just sol <- m ^. solution -> zonk sol
        _ -> pure ty
    zonk (a :*: b)    = (:*:) <$> zonk a <*> zonk b
    zonk (a :+: b)    = (:+:) <$> zonk a <*> zonk b
    zonk (a :->: b)   = (:->:) <$> zonk a <*> zonk b
    zonk (Later t)    = Later <$> zonk t
    zonk (Constant t) = Constant <$> zonk t
    zonk (TFix x t)   = typeCtx %~ ((x, False):) |- TFix x <$> zonk t
    zonk ty           = pure ty

instance Zonk Polytype where
    zonk (Forall xs ty) = typeCtx %~ (xs ++) |- Forall xs <$> zonk ty

-- | Instantiate a polytype to a type by replacing polymorphic type variables with fresh metavariables.
instantiate :: MonadCheckTerm m => Polytype -> m Type
instantiate (Monotype ty) = pure ty
instantiate (Forall xs ty) = do
    s <- for xs \(x, c) -> (x,) <$> newMeta' c
    pure $ substituteType (Map.fromList s) ty

-- | Generalise a type to a polytype by abstracting over its metavariables that aren't free in the context.
generalise :: MonadCheckTerm m => Type -> m Polytype
generalise ty = do
    ty <- zonk ty
    freeInCtx <- fmap (foldMap freeTVars) . traverse (zonk . fst) =<< view termCtx
    metas <- use metas
    let generalisable = metas `Map.restrictKeys` freeTVars ty `Map.withoutKeys` freeInCtx
    pure $ Forall [(x, m ^. constant) | (x, m) <- Map.toList generalisable] ty

class Constant t where
    -- | Check whether a type or term is constant, optionally forcing it to be by marking its metavariables as constant.
    isConstant :: MonadCheckTerm m => Bool -> t -> m Constancy

-- | A type is constant if all uses of @▸@ occur under @■@.
instance Constant Type where
    isConstant force (TVar x) = viewTVar x >>= \case
        Left m | Just sol <- m ^. solution ->
                    if m ^. constant then pure True else isConstant force sol
               | force -> metas.ix x.constant <.= True
               | otherwise -> pure (m ^. constant)
        Right c -> c <$ when (force && not c) (throwError $ "non-constant type variable " ++ x)
    isConstant force (t1 :*: t2) = (&&) <$> isConstant force t1 <*> isConstant force t2
    isConstant force (t1 :+: t2) = (&&) <$> isConstant force t1 <*> isConstant force t2
    isConstant force (_ :->: t2) = isConstant force t2
    isConstant force ty@Later{}
        | force                    = throwError $ "non-constant type " ++ show ty
        | otherwise                = pure False
    isConstant force (TFix x t)  = typeCtx %~ ((x, False):) |- isConstant force t -- if the type is well-formed, we'll never encounter x
    isConstant _     _           = pure True

instance Constant Polytype where
    isConstant force (Forall xs ty) = typeCtx %~ (xs ++) |- isConstant force ty

-- | A term is constant if it only refers to constant terms or terms with a constant type.
instance Constant Term where
    isConstant force t = do
        ctx <- view termCtx
        and <$> traverse constantBinding (ctx `Map.restrictKeys` freeVars t)
        where
        constantBinding (_, True) = pure True
        constantBinding (ty, False) = isConstant force ty

-- * Unification

infix 5 !:=

-- | Attempt to assign a type to a metavariable.
-- Performs occurs check, scope check and constancy check.
(!:=) :: MonadCheckTerm m => (TVar, Meta) -> Type -> m ()
(x, meta) !:= ty
    | Just sol <- meta ^. solution = sol !~ ty
    | otherwise = assign =<< zonk ty where
    assign ty
        | ty == TVar x = pure ()
        | x `freeInType` ty = throwError $ "cannot construct infinite type " ++ x ++ " ~ " ++ show ty
        | otherwise = do
            ctx <- view typeCtx
            let escaped = [x | x <- Set.toList (freeTVars ty)
                             , Just (_, l) <- [lookupLevel x ctx]
                             , l >= meta ^. level]
            unless (null escaped) $ throwError $
                "cannot unify " ++ x ++ " with " ++ show ty ++ ": type variables " ++ show (map TVar escaped) ++ " would escape their scope"
            when (meta ^. constant) $ () <$ isConstant True ty
            metas.ix x.solution ?= ty

infix 4 !~

-- | Unify two types.
(!~) :: MonadCheckTerm m => Type -> Type -> m ()
ta1 :*: tb1  !~ ta2 :*: tb2      = ta1 !~ ta2 >> tb1 !~ tb2
ta1 :+: tb1  !~ ta2 :+: tb2      = ta1 !~ ta2 >> tb1 !~ tb2
ta1 :->: tb1 !~ ta2 :->: tb2     = ta1 !~ ta2 >> tb1 !~ tb2
Later ty1    !~ Later ty2        = ty1 !~ ty2
Constant ty1 !~ Constant ty2     = ty1 !~ ty2
TFix x1 tf1  !~ TFix x2 tf2      = typeCtx %~ ((x1, False):) |- tf1 !~ substituteType1 x2 (TVar x1) tf2
ty1          !~ ty2              = ifMeta ty1 (!:= ty2) $ ifMeta ty2 (!:= ty1) $ unless (ty1 == ty2) do
    ty1 <- zonk ty1
    ty2 <- zonk ty2
    throwError $ "cannot match type " ++ show ty1 ++ " with " ++ show ty2

-- * Type checking and inference

-- | The type of the integer recursion operator.
intrecType :: Polytype
intrecType = Forall [("a", False)] (("a" :->: "a") :->: "a" :->: ("a" :->: "a") :->: TInt :->: "a")

class Types t where
    infix 4 !:

    -- | Check that a term has the given type.
    (!:) :: MonadCheckTerm m => Term -> t -> m ()

    -- | Infer a type for the given term.
    (?:) :: MonadCheckTerm m => Term -> m t

instance Types Type where
    Var x !: ty = view (termCtx.at x) >>= \case
        Just (tyx, _) -> (ty !~) =<< instantiate tyx
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
        ~[ta, tb] <- newMetas 2
        ty !~ ta :*: tb
        a !: ta
        b !: tb
    Fst t !: ty = do
        tb <- newMeta
        t !: ty :*: tb
    Snd t !: ty = do
        ta <- newMeta
        t !: ta :*: ty
    Abort t !: _ = t !: Zero
    InL t !: ty = do
        ~[ta, tb] <- newMetas 2
        ty !~ ta :+: tb
        t !: ta
    InR t !: ty = do
        ~[ta, tb] <- newMetas 2
        ty !~ ta :+: tb
        t !: tb
    Case t ~(Abs x1 t1) ~(Abs x2 t2) !: ty = do
        ~[ta, tb] <- newMetas 2
        t !: ta :+: tb
        constant <- isConstant False t
        termCtx.at x1 ?~ (Monotype ta, constant) |- t1 !: ty
        termCtx.at x2 ?~ (Monotype tb, constant) |- t2 !: ty
    Abs x t !: ty = do
        ~[ta, tb] <- newMetas 2
        ty !~ ta :->: tb
        termCtx.at x ?~ (Monotype ta, False) |- t !: tb
    s :$: t !: ty = do
        ta <- newMeta
        s !: ta :->: ty
        t !: ta
    Let s t !: ty = do
        e <- for s \t' -> do
            ty <- (t' ?:)
            constant <- isConstant False t'
            pure (ty, constant)
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
        ta <- newMeta
        ty !~ Later ta
        t !: ta
    Prev t !: ty = do
        t !: Later ty
        () <$ isConstant True t
    s :<*>: t !: ty = do
        ~[ta, tb] <- newMetas 2
        ty !~ Later ta
        t !: Later tb
        s !: Later (tb :->: ta)
    Box t !: ty = do
        ta <- newMeta
        ty !~ Constant ta
        t !: ta
        () <$ isConstant True t
    Unbox t !: ty = t !: Constant ty

    (?:) t = do
        ty <- newMeta
        t !: ty
        pure ty

instance Types Polytype where
    t !: Forall xs ty = typeCtx %~ (xs ++) |- t !: ty

    (?:) = (?:) >=> generalise

checkTerm t ty = runUnification (t !: ty) (allTVars ty)

inferTerm t = alphaNormalise <$> runUnification (t ?:) mempty
