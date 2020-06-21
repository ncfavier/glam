module Glam.Term where

import           Control.Monad (replicateM)
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

type Var = String

type Subst = Map Var Term

infixl :<*>:
infixl :$:

data Term =
    -- Variables
      Var Var
    -- Natural numbers
    | Zero | Succ Term
    -- Products
    | Unit | Pair Term Term | Fst Term | Snd Term
    -- Sums
    | Abort Term | InL Term | InR Term | Case Term Term Term
    -- Functions
    | Abs Var Term | Term :$: Term
    -- Recursion operations
    | Fold Term | Unfold Term | Fix Term
    -- 'Later' operations
    | Next Term | Prev Delayed | Term :<*>: Term
    -- 'Constant' operations
    | Box Delayed | Unbox Term
    deriving Eq

data Delayed = Subst Subst Term deriving Eq

-- Variables

freeVariables :: Term -> Set Var
freeVariables (Var x)        = Set.singleton x
freeVariables Zero           = Set.empty
freeVariables (Succ t)       = freeVariables t
freeVariables Unit           = Set.empty
freeVariables (Pair t1 t2)   = freeVariables t1 <> freeVariables t2
freeVariables (Fst t)        = freeVariables t
freeVariables (Snd t)        = freeVariables t
freeVariables (Abort t)      = freeVariables t
freeVariables (InL t)        = freeVariables t
freeVariables (InR t)        = freeVariables t
freeVariables (Case t t1 t2) = freeVariables t <> freeVariables t1 <> freeVariables t2
freeVariables (Abs x t)      = Set.delete x (freeVariables t)
freeVariables (t1 :$: t2)    = freeVariables t1 <> freeVariables t2
freeVariables (Fold t)       = freeVariables t
freeVariables (Unfold t)     = freeVariables t
freeVariables (Fix t)        = freeVariables t
freeVariables (Next t)       = freeVariables t
freeVariables (Prev d)       = freeVariablesDelayed d
freeVariables (t1 :<*>: t2)  = freeVariables t1 <> freeVariables t2
freeVariables (Box d)        = freeVariablesDelayed d
freeVariables (Unbox t)      = freeVariables t

freeVariablesSubst :: Subst -> Set Var
freeVariablesSubst = foldMap freeVariables

freeVariablesDelayed :: Delayed -> Set Var
freeVariablesDelayed (Subst s t) = freeVariablesSubst s <> (freeVariables t Set.\\ Map.keysSet s)

freshFor :: Set Var -> [Var]
freshFor vs = [v | n <- [1..]
                 , v <- replicateM n ['a'..'z']
                 , v `Set.notMember` vs]

-- Alpha-renames an abstraction so as to avoid capturing any of the variables in `vs`.
avoidCapture vs t@(Abs x t')
    | x `Set.member` vs = Abs y (substitute1 x (Var y) t')
    | otherwise = t
    where y:_ = freshFor (vs <> freeVariables t)

-- Same for a delayed substitution.
avoidCaptureDelayed vs d@(Subst s t)
    | not (null conflicts) = Subst s' t'
    | otherwise = d
    where conflicts = Map.keys (Map.restrictKeys s vs)
          renaming = Map.fromList (zip conflicts (freshFor (vs <> freeVariablesDelayed d <> Map.keysSet s)))
          s' = Map.mapKeys (\v -> Map.findWithDefault v v renaming) s
          t' = substitute (Map.map Var renaming) t

-- Capture-avoiding substitution.
substitute :: Subst -> Term -> Term
substitute s (Var x)        = Map.findWithDefault (Var x) x s
substitute s Zero           = Zero
substitute s (Succ t)       = Succ (substitute s t)
substitute s Unit           = Unit
substitute s (Pair t1 t2)   = Pair (substitute s t1) (substitute s t2)
substitute s (Fst t)        = Fst (substitute s t)
substitute s (Snd t)        = Snd (substitute s t)
substitute s (Abort t)      = Abort (substitute s t)
substitute s (InL t)        = InL (substitute s t)
substitute s (InR t)        = InR (substitute s t)
substitute s (Case t t1 t2) = Case (substitute s t) (substitute s t1) (substitute s t2)
substitute s t@Abs{}        = Abs x (substitute (Map.delete x s) t')
    where Abs x t' = avoidCapture (freeVariablesSubst s) t
substitute s (t1 :$: t2)    = substitute s t1 :$: substitute s t2
substitute s (Fold t)       = Fold (substitute s t)
substitute s (Unfold t)     = Unfold (substitute s t)
substitute s (Fix t)        = Fix (substitute s t)
substitute s (Next t)       = Next (substitute s t)
substitute s (Prev d)       = Prev (substituteDelayed s d)
substitute s (t1 :<*>: t2)  = substitute s t1 :<*>: substitute s t2
substitute s (Box d)        = Box (substituteDelayed s d)
substitute s (Unbox t)      = Unbox (substitute s t)

substituteDelayed :: Subst -> Delayed -> Delayed
substituteDelayed s d = Subst (substitute s <$> s') (substitute (s Map.\\ s') t)
    where Subst s' t = avoidCaptureDelayed (freeVariablesSubst s) d

substitute1 x s = substitute (Map.singleton x s)

-- Identity substitutions for `prev` and `box`.
prev t = Prev (Subst (Map.fromSet Var (freeVariables t)) t)
box t = Box (Subst (Map.fromSet Var (freeVariables t)) t)

-- Small-step operational semantics for the calculus: performs a single reduction step, if possible.
reduce :: Term -> Maybe Term
-- Reduction rules
reduce (Abs x s :$: t)                        = Just (substitute1 x t s)
reduce (Fst (Pair t1 _))                      = Just t1
reduce (Snd (Pair _ t2))                      = Just t2
reduce (Case (InL t) (Abs x1 t1) _)           = Just (substitute1 x1 t t1)
reduce (Case (InR t) _ (Abs x2 t2))           = Just (substitute1 x2 t t2)
reduce (Unfold (Fold t))                      = Just t
reduce t@(Fix (Abs x t'))                     = Just (substitute1 x (Next t) t')
reduce (Next t1 :<*>: Next t2)                = Just (Next (t1 :$: t2))
reduce (Unbox (Box (Subst s t)))              = Just (substitute s t)
reduce (Prev (Subst s (Next t))) | Map.null s = Just t
reduce (Prev (Subst s t)) | not (Map.null s)  = Just (Prev (Subst Map.empty (substitute s t)))
-- Context rules (weak call-by-name evaluation)
reduce (t1 :$: t2)        | Just t1' <- reduce t1           = Just (t1' :$: t2)
reduce (InL t)            | Just t' <- reduce t             = Just (InL t')
reduce (InR t)            | Just t' <- reduce t             = Just (InR t')
reduce (Case t t1 t2)     | Just t' <- reduce t             = Just (Case t' t1 t2)
reduce (Unfold t)         | Just t' <- reduce t             = Just (Unfold t')
reduce (Unbox t)          | Just t' <- reduce t             = Just (Unbox t')
reduce (Prev (Subst s t)) | Map.null s, Just t' <- reduce t = Just (Prev (Subst s t'))
reduce (t1 :<*>: t2)      | Just t1' <- reduce t1           = Just (t1' :<*>: t2)
reduce (t1 :<*>: t2)      | Just t2' <- reduce t2           = Just (t1 :<*>: t2')
reduce _ = Nothing

-- Reduce a term to a (weak) normal form.
normalise :: Term -> Term
normalise t = maybe t normalise (reduce t)

termToNat :: Term -> Maybe Integer
termToNat Zero     = Just 0
termToNat (Succ t) = succ <$> termToNat t
termToNat _        = Nothing

-- Printing

showSubst s = intercalate "; " [v ++ " = " ++ show t | (v, t) <- Map.assocs s]

pad "" = ""
pad s = " " ++ s ++ " "

appPrec = 10

instance Show Term where
    showsPrec _ (Var x) = showString x
    showsPrec _ Zero = shows 0
    showsPrec d t@(Succ t')
        | Just n <- termToNat t = shows n
        | otherwise = showParen (d > appPrec) $ showString "succ " . showsPrec (appPrec + 1) t'
    showsPrec d Unit = showString "()"
    showsPrec d (Pair t1 t2) = showParen True $ shows t1 . showString ", " . shows t2
    showsPrec d (Fst t) = showParen (d > appPrec) $ showString "fst " . showsPrec (appPrec + 1) t
    showsPrec d (Snd t) = showParen (d > appPrec) $ showString "snd " . showsPrec (appPrec + 1) t
    showsPrec d (Abort t) = showParen (d > appPrec) $ showString "abort " . showsPrec (appPrec + 1) t
    showsPrec d (InL t) = showParen (d > appPrec) $ showString "left " . showsPrec (appPrec + 1) t
    showsPrec d (InR t) = showParen (d > appPrec) $ showString "right " . showsPrec (appPrec + 1) t
    showsPrec d (Case t (Abs x1 t1) (Abs x2 t2)) = showParen (d > 0) $
        showString "case " . shows t . showString " of { left "
            . showString x1 . showString " -> " . shows t1
            . showString "; right "
            . showString x2 . showString " -> " . shows t2
            . showString " }"
    showsPrec d (Abs x t) = showParen (d > 0) $ showChar '\\' . showString x . showString " -> " . shows t
    showsPrec d (t1 :$: t2) = showParen (d > appPrec) $ showsPrec appPrec t1 . showChar ' ' . showsPrec (appPrec + 1) t2
    showsPrec d (Fold t) = showParen (d > appPrec) $ showString "fold " . showsPrec (appPrec + 1) t
    showsPrec d (Unfold t) = showParen (d > appPrec) $ showString "unfold " . showsPrec (appPrec + 1) t
    showsPrec d (Fix (Abs x t)) = showParen (d > 0) $ showString "fix " . showString x . showString " -> " . shows t
    showsPrec d (Next t) = showParen (d > appPrec) $ showString "next " . showsPrec (appPrec + 1) t
    showsPrec d (Prev (Subst s t)) = showParen (d > 0) $
        showString "prev {" . showString (pad (showSubst s)) . showString "} in " . shows t
    showsPrec d (t1 :<*>: t2) = showParen (d > prec) $ showsPrec prec t1 . showString " <*> " . showsPrec (prec + 1) t2
        where prec = 4
    showsPrec d (Box (Subst s t)) = showParen (d > 0) $
        showString "box {" . showString (pad (showSubst s)) . showString "} in " . shows t
    showsPrec d (Unbox t) = showParen (d > appPrec) $ showString "unbox " . showsPrec (appPrec + 1) t

-- Parsing: TODO