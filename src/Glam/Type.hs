module Glam.Type where

import           Data.Set (Set)
import qualified Data.Set as Set

type TVar = String

data Type =
    -- Type variables
      TVar TVar
    -- Products
    | One | Type :*: Type
    -- Sums
    | Type :+: Type
    -- Functions
    | Type :->: Type
    -- Modalities
    | Later Type | Now Type
    -- Fixed points
    | TFix TVar Type
    deriving Eq

freeTypeVariables :: Type -> Set TVar
freeTypeVariables (TVar x)     = Set.singleton x
freeTypeVariables One          = Set.empty
freeTypeVariables (t1 :*: t2)  = freeTypeVariables t1 <> freeTypeVariables t2
freeTypeVariables (t1 :+: t2)  = freeTypeVariables t1 <> freeTypeVariables t2
freeTypeVariables (t1 :->: t2) = freeTypeVariables t1 <> freeTypeVariables t2
freeTypeVariables (Later t)    = freeTypeVariables t
freeTypeVariables (Now t)      = freeTypeVariables t
freeTypeVariables (TFix x t)   = Set.delete x (freeTypeVariables t)

isGuarded :: TVar -> Type -> Bool
isGuarded x (TVar y)      = x /= y
isGuarded _ One          = True
isGuarded x (t1 :*: t2)  = isGuarded x t1 && isGuarded x t2
isGuarded x (t1 :+: t2)  = isGuarded x t1 && isGuarded x t2
isGuarded x (t1 :->: t2) = isGuarded x t1 && isGuarded x t2
isGuarded _ (Later t)    = True
isGuarded x (Now t)      = isGuarded x t
isGuarded x (TFix y t)   = x == y || isGuarded x t

-- Valid types.
isValid :: Type -> Bool
isValid (TVar v)     = True
isValid One          = True
isValid (t1 :*: t2)  = isValid t1 && isValid t2
isValid (t1 :+: t2)  = isValid t1 && isValid t2
isValid (t1 :->: t2) = isValid t1 && isValid t2
isValid (Later t)    = isValid t
isValid (Now t)      = isValid t && Set.null (freeTypeVariables t)
isValid (TFix x t)   = isValid t && isGuarded x t

-- Constant types.
isConstant :: Type -> Bool
isConstant (TVar v)     = True
isConstant One          = True
isConstant (t1 :*: t2)  = isConstant t1 && isConstant t2
isConstant (t1 :+: t2)  = isConstant t1 && isConstant t2
isConstant (t1 :->: t2) = isConstant t1 && isConstant t2
isConstant (Later t)    = False
isConstant (Now t)      = True
isConstant (TFix _ t)   = isConstant t

-- Printing

prodPrec = 6
sumPrec = 4
funPrec = 0
modPrec = 8

instance Show Type where
    showsPrec _ (TVar x) = showString x
    showsPrec _ One = showString "1"
    showsPrec d (t1 :*: t2) = showParen (d > prodPrec) $ showsPrec (prodPrec + 1) t1 . showString " * " . showsPrec (prodPrec + 1) t2
    showsPrec d (t1 :+: t2) = showParen (d > sumPrec) $ showsPrec (sumPrec + 1) t1 . showString " + " . showsPrec (sumPrec + 1) t2
    showsPrec d (t1 :->: t2) = showParen (d > funPrec) $ showsPrec (funPrec + 1) t1 . showString " -> " . showsPrec funPrec t2
    showsPrec d (Later t) = showParen (d > modPrec) $ showString ">" . showsPrec modPrec t
    showsPrec d (Now t) = showParen (d > modPrec) $ showString "#" . showsPrec modPrec t
    showsPrec d (TFix x t) = showParen (d > 0) $ showString "Fix " . showString x . showString " -> " . shows t

-- Parsing: TODO
