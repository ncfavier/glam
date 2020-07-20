module Glam.Type where

import           Data.Set (Set)
import qualified Data.Set as Set
import           Control.Monad

import Glam.Parser

type TVar = String

infixr 7 :*:
infixr 6 :+:

data Type =
    -- Type variables
      TVar TVar
    -- Base types
    | TInt
    -- Type applications
    -- | TApp Type Type
    -- Products
    | One | Type :*: Type
    -- Sums
    | Zero | Type :+: Type
    -- Functions
    | Type :->: Type
    -- Modalities
    | Later Type | Constant Type
    -- Fixed points
    | TFix TVar Type
    deriving Eq

data TypeConstructor = TAbs [TVar] Type
    deriving (Eq, Show)

freeTypeVariables :: Type -> Set TVar
freeTypeVariables (TVar x)     = Set.singleton x
freeTypeVariables (t1 :*: t2)  = freeTypeVariables t1 <> freeTypeVariables t2
freeTypeVariables (t1 :+: t2)  = freeTypeVariables t1 <> freeTypeVariables t2
freeTypeVariables (t1 :->: t2) = freeTypeVariables t1 <> freeTypeVariables t2
freeTypeVariables (Later t)    = freeTypeVariables t
freeTypeVariables (Constant t) = freeTypeVariables t
freeTypeVariables (TFix x t)   = Set.delete x (freeTypeVariables t)
freeTypeVariables _            = Set.empty

freeInType :: TVar -> Type -> Bool
x `freeInType` t = x `Set.member` freeTypeVariables t

isClosed :: Type -> Bool
isClosed t = Set.null (freeTypeVariables t)

guardedIn :: TVar -> Type -> Bool
x `guardedIn` TVar y       = x /= y
_ `guardedIn` TInt         = True
_ `guardedIn` One          = True
x `guardedIn` (t1 :*: t2)  = x `guardedIn` t1 && x `guardedIn` t2
_ `guardedIn` Zero         = True
x `guardedIn` (t1 :+: t2)  = x `guardedIn` t1 && x `guardedIn` t2
x `guardedIn` (t1 :->: t2) = x `guardedIn` t1 && x `guardedIn` t2
_ `guardedIn` Later _      = True
x `guardedIn` Constant t   = x `guardedIn` t
x `guardedIn` TFix y t     = x == y || x `guardedIn` t

-- Valid types
isValid :: Type -> Bool
isValid (t1 :*: t2)  = isValid t1 && isValid t2
isValid (t1 :+: t2)  = isValid t1 && isValid t2
isValid (t1 :->: t2) = isValid t1 && isValid t2
isValid (Later t)    = isValid t
isValid (Constant t) = isValid t && isClosed t
isValid (TFix x t)   = isValid t && x `guardedIn` t
isValid _            = True

-- Constant types
isConstant :: Type -> Bool
isConstant (t1 :*: t2) = isConstant t1 && isConstant t2
isConstant (t1 :+: t2) = isConstant t1 && isConstant t2
isConstant (_ :->: t2) = isConstant t2
isConstant (Later _)   = False
isConstant (TFix _ t)  = isConstant t
isConstant _           = True

freshTVarFor :: Set TVar -> [TVar]
freshTVarFor vs = [v | n <- [1..]
                     , v <- replicateM n ['a'..'z']
                     , v `Set.notMember` vs]

avoidCaptureType vs (TFix x t)
    | x `Set.member` vs = TFix y (substituteType x (TVar y) t)
    | otherwise = TFix x t
    where y:_ = freshTVarFor (vs <> freeTypeVariables (TFix x t))

substituteType :: TVar -> Type -> Type -> Type
substituteType x t (TVar y) | x == y = t
substituteType x t (t1 :*: t2) = substituteType x t t1 :*: substituteType x t t2
substituteType x t (t1 :+: t2) = substituteType x t t1 :+: substituteType x t t2
substituteType x t (t1 :->: t2) = substituteType x t t1 :->: substituteType x t t2
substituteType x t (Later t1) = Later (substituteType x t t1)
substituteType x t (Constant t1) = Constant (substituteType x t t1)
substituteType x t (TFix y tf)
    | x == y = TFix y tf
    | otherwise = TFix y' (substituteType x t tf')
    where TFix y' tf' = avoidCaptureType (freeTypeVariables t) (TFix y tf)
substituteType _ _ ty = ty

-- Printing

prodPrec = 6
sumPrec = 4
funPrec = 0
modPrec = 8

instance Show Type where
    showsPrec _ (TVar x)     = showString x
    showsPrec _ TInt         = showString "Int"
    showsPrec _ One          = showString "1"
    showsPrec d (t1 :*: t2)  = showParen (d > prodPrec) $
        showsPrec (prodPrec + 1) t1 . showString " * " . showsPrec (prodPrec + 1) t2
    showsPrec _ Zero         = showString "0"
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
typeVariable = mkIdentifier ["Fix", "Int"]

type_ :: Parser Type
type_ = tfix <|> makeExprParser base ops <?> "type"
    where
    tfix = flip (foldr TFix) <$ "Fix" <*> some typeVariable <* dot <*> type_
    base =  TInt <$ "Int"
        <|> TVar <$> typeVariable
        <|> One <$ symbol "1"
        <|> Zero <$ symbol "0"
        <|> parens type_
    modality =  Later <$ symbol ">"
            <|> Constant <$ symbol "#"
    ops = [ {-[InfixL (pure TApp)]
          , -}[Prefix (foldr1 (.) <$> some modality)]
          , [binary "*" (:*:)]
          , [binary "+" (:+:)]
          , [binary "->" (:->:)] ]
    binary w f = InfixR (f <$ symbol w)

-- typeDef :: Parser (TVar, TypeConstructor)
-- typeDef = mkTypeDef <$ "type" <*> typeVariable <*> many typeVariable <* equal <*> type_
--     where
--     mkTypeDef x ys t = TAbs ys (autoTFix t) where
--         rec =
--         autoTFix t | x `freeIn` t = (x, Fix (Abs x t))
--                    | otherwise    = (x, t)
