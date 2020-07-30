module Glam.Type where

import           Data.List
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Map (Map)
import qualified Data.Map as Map
import           Control.Monad

import Glam.Parser

type TVar = String

type TSubst = Map TVar Type

infixr 7 :*:
infixr 6 :+:

data Type =
    -- Type variables
      TVar TVar
    -- Base types
    | TInt
    -- Type applications
    | TApp Type Type
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

data Polytype = Forall [(TVar, Bool)] Type
              deriving Eq

pattern Monotype ty = Forall [] ty

class Types t where
    isValid :: t -> Bool
    freeTVars :: t -> Set TVar

instance Types Type where
    isValid (t1 :*: t2)  = isValid t1 && isValid t2
    isValid (t1 :+: t2)  = isValid t1 && isValid t2
    isValid (t1 :->: t2) = isValid t1 && isValid t2
    isValid (Later t)    = isValid t
    isValid (Constant t) = isValid t && isClosed t
    isValid (TFix x t)   = isValid t && x `guardedIn` t
    isValid _            = True

    freeTVars (TVar x)     = Set.singleton x
    freeTVars (t1 :*: t2)  = freeTVars t1 <> freeTVars t2
    freeTVars (t1 :+: t2)  = freeTVars t1 <> freeTVars t2
    freeTVars (t1 :->: t2) = freeTVars t1 <> freeTVars t2
    freeTVars (Later t)    = freeTVars t
    freeTVars (Constant t) = freeTVars t
    freeTVars (TFix x t)   = Set.delete x (freeTVars t)
    freeTVars _            = Set.empty

instance Types Polytype where
    isValid (Forall _ ty) = isValid ty
    freeTVars (Forall (map fst -> xs) ty) = freeTVars ty Set.\\ Set.fromList xs

x `freeInType` t = x `Set.member` freeTVars t

isClosed t = Set.null (freeTVars t)

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

freshTVarFor :: Set TVar -> [TVar]
freshTVarFor vs = [v | n <- [1..]
                     , v <- replicateM n ['a'..'z']
                     , v `Set.notMember` vs]

avoidCaptureType vs ty@(TFix x tf)
    | x `Set.member` vs = TFix y (substituteType1 x (TVar y) tf)
    | otherwise = ty
    where y:_ = freshTVarFor (vs <> freeTVars ty)

substituteType :: TSubst -> Type -> Type
substituteType s (TVar x) = Map.findWithDefault (TVar x) x s
substituteType s (t1 :*: t2) = substituteType s t1 :*: substituteType s t2
substituteType s (t1 :+: t2) = substituteType s t1 :+: substituteType s t2
substituteType s (t1 :->: t2) = substituteType s t1 :->: substituteType s t2
substituteType s (Later t1) = Later (substituteType s t1)
substituteType s (Constant t1) = Constant (substituteType s t1)
substituteType s ty@TFix{} = TFix x (substituteType s tf)
    where TFix x tf = avoidCaptureType (foldMap freeTVars s) ty
substituteType _ ty = ty

substituteType1 x s = substituteType (Map.singleton x s)

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
    showsPrec d (Later ty)    = showParen (d > modPrec) $
        showString ">" . showsPrec modPrec ty
    showsPrec d (Constant ty) = showParen (d > modPrec) $
        showString "#" . showsPrec modPrec ty
    showsPrec _ (TFix x tf)   = showParen True $
        showString "Fix " . showString x . showString ". " . shows tf

instance Show Polytype where
    showsPrec _ (Forall [] ty) = shows ty
    showsPrec _ (Forall xs ty) = showString "forall " . showString (intercalate " " [(if c then "#" else "") ++ x | (x, c) <- xs]) . showString ". " . shows ty

-- Parsing

tVar :: Parser TVar
tVar = mkIdentifier ["Fix", "Int", "forall"]

type_ :: Parser Type
type_ = tfix <|> makeExprParser base ops <?> "type"
    where
    tfix = flip (foldr TFix) <$ "Fix" <*> some tVar <* dot <*> type_
    base =  TInt <$ "Int"
        <|> TVar <$> tVar
        <|> One <$ symbol "1"
        <|> Zero <$ symbol "0"
        <|> parens type_
    modality =  Later <$ symbol ">"
            <|> Constant <$ symbol "#"
    ops = [ [InfixL (pure TApp)]
          , [Prefix (foldr1 (.) <$> some modality)]
          , [binary "*" (:*:)]
          , [binary "+" (:+:)]
          , [binary "->" (:->:)] ]
    binary w f = InfixR (f <$ symbol w)

quantifiedTVar :: Parser (TVar, Bool)
quantifiedTVar = flip (,) <$> option False (True <$ symbol "#") <*> tVar

polytype :: Parser Polytype
polytype = Forall <$> option [] ("forall" *> some quantifiedTVar <* dot) <*> type_
