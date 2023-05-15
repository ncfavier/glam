{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Glam.Rules.Type (TEnvironment, TVarBinding(..), checkPolytype, checkTypeSynonym) where

import Data.Map (Map)
import Data.Map qualified as Map
import Control.Monad.Except hiding (guard)
import Control.Monad.Reader hiding (guard)
import Control.Monad.Writer hiding (guard)
import Control.Lens

import Glam.Utils
import Glam.Type

data TVarBinding = Syn [TVar] Type
                 | Self { _guardedness :: Guardedness, arguments :: Maybe [TVar] }
                 | Bound

makeLenses ''TVarBinding

type TEnvironment = Map TVar TVarBinding

type MonadCheckType m = (MonadReader TEnvironment m, MonadError String m)

guard :: Guardedness -> Guardedness
guard Unguarded = Guarded
guard x = x

checkType :: MonadCheckType m => Type -> m (Type, First TVar)
checkType = runWriterT . go where
  go ty@TVar{}     = apply ty []
  go ty@TApp{}     = apply ty []
  go (ta :*: tb)   = (:*:) <$> go ta <*> go tb
  go (ta :+: tb)   = (:+:) <$> go ta <*> go tb
  go (ta :->: tb)  = (:->:) <$> go ta <*> go tb
  go (Later ty)    = mapped.guardedness %~ guard |- Later <$> go ty
  go (Constant ty) = mapped.guardedness .~ Forbidden |- Constant <$> go ty
  go (TFix x tf)   = at x ?~ Self Unguarded Nothing |- TFix x <$> go tf
  go ty            = pure ty
  apply (TApp t1 t2) tys = do
      t2' <- go t2
      apply t1 (t2':tys)
  apply (TVar x) tys = view (at x) >>= maybe (throwError $ "unbound type variable " ++ x) \case
      Syn ys ty
          | length ys == length tys -> pure $ substituteType (Map.fromList (zip ys tys)) ty
          | otherwise -> throwError $
              "wrong number of arguments for type constructor " ++ x ++ ": got " ++
              show (length tys) ++ ", expecting " ++ show (length ys)
      Self _ (Just ys) | tys /= map TVar ys -> throwError $
          "recursive type constructor " ++ x ++ " must be applied to the same arguments"
      Self Unguarded _ -> throwError $ "unguarded fixed point variable " ++ x
      Self Forbidden _ -> throwError $ "fixed point variable " ++ x ++ " cannot appear under #"
      Self Guarded (Just _) -> TVar x <$ tell (pure x)
      Self Guarded Nothing -> pure (TVar x)
      Bound | null tys -> pure (TVar x)
            | otherwise -> throwError $ "not a type constructor: " ++ x
  apply ty _ = throwError $ "not a type constructor: " ++ show ty

checkPolytype :: MonadCheckType m => Polytype -> m Polytype
checkPolytype (Forall as ty) = do
    let scope = Map.fromList [(a, Bound) | (a, _) <- as]
    (ty', _) <- Map.union scope |- checkType ty
    pure $ Forall as ty'

checkTypeSynonym :: MonadCheckType m => TVar -> [TVar] -> Type -> m Type
checkTypeSynonym x ys ty = do
    let scope = Map.fromList $ (x, Self Unguarded (Just ys)) : [(y, Bound) | y <- ys]
    (ty', First autofix) <- Map.union scope |- checkType ty
    pure $ maybe id TFix autofix ty'
