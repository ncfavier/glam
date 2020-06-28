{-# LANGUAGE OverloadedStrings #-}
module Glam.Type where

import           Data.Set (Set)
import qualified Data.Set as Set

import Glam.Parse

type TVar = String

infixr 7 :*:
infixr 6 :+:

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
    | Later Type | Constant Type
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
freeTypeVariables (Constant t) = freeTypeVariables t
freeTypeVariables (TFix x t)   = Set.delete x (freeTypeVariables t)

guardedIn :: TVar -> Type -> Bool
x `guardedIn` TVar y       = x /= y
_ `guardedIn` One          = True
x `guardedIn` (t1 :*: t2)  = x `guardedIn` t1 && x `guardedIn` t2
x `guardedIn` (t1 :+: t2)  = x `guardedIn` t1 && x `guardedIn` t2
x `guardedIn` (t1 :->: t2) = x `guardedIn` t1 && x `guardedIn` t2
_ `guardedIn` Later _      = True
x `guardedIn` Constant t   = x `guardedIn` t
x `guardedIn` TFix y t     = x == y || x `guardedIn` t

-- Valid types.
isValid :: Type -> Bool
isValid (TVar _)     = True
isValid One          = True
isValid (t1 :*: t2)  = isValid t1 && isValid t2
isValid (t1 :+: t2)  = isValid t1 && isValid t2
isValid (t1 :->: t2) = isValid t1 && isValid t2
isValid (Later t)    = isValid t
isValid (Constant t) = isValid t && Set.null (freeTypeVariables t)
isValid (TFix x t)   = isValid t && x `guardedIn` t

-- Constant types.
isConstant :: Type -> Bool
isConstant (TVar _)     = True
isConstant One          = True
isConstant (t1 :*: t2)  = isConstant t1 && isConstant t2
isConstant (t1 :+: t2)  = isConstant t1 && isConstant t2
isConstant (t1 :->: t2) = isConstant t1 && isConstant t2
isConstant (Later _)    = False
isConstant (Constant _) = True
isConstant (TFix _ t)   = isConstant t

-- Printing

prodPrec = 6
sumPrec = 4
funPrec = 0
modPrec = 8

instance Show Type where
    showsPrec _ (TVar x)     = showString x
    showsPrec _ One          = showString "1"
    showsPrec d (t1 :*: t2)  = showParen (d > prodPrec) $
        showsPrec (prodPrec + 1) t1 . showString " * " . showsPrec (prodPrec + 1) t2
    showsPrec d (t1 :+: t2)  = showParen (d > sumPrec) $
        showsPrec (sumPrec + 1) t1 . showString " + " . showsPrec (sumPrec + 1) t2
    showsPrec d (t1 :->: t2) = showParen (d > funPrec) $
        showsPrec (funPrec + 1) t1 . showString " -> " . showsPrec funPrec t2
    showsPrec d (Later t)    = showParen (d > modPrec) $
        showString ">" . showsPrec modPrec t
    showsPrec d (Constant t) = showParen (d > modPrec) $
        showString "#" . showsPrec modPrec t
    showsPrec d (TFix x t)   = showParen (d > 0) $
        showString "Fix " . showString x . showString ". " . shows t

-- Parsing

typeVariable :: Parser TVar
typeVariable = mkIdentifier ["Fix"]

type_ :: Parser Type
type_ = tfix <|> makeExprParser base ops <?> "type"
    where
    tfix = flip (foldr TFix) <$ "Fix" <*> some typeVariable <* dot <*> type_
    base = TVar <$> typeVariable <|> One <$ symbol "1" <|> parens type_
    modality = Later <$ symbol ">" <|> Constant <$ symbol "#"
    ops = [ [Prefix (foldr1 (.) <$> some modality)]
          , [binary "*" (:*:)]
          , [binary "+" (:+:)]
          , [binary "->" (:->:)] ]
    binary w f = InfixR (f <$ symbol w)
