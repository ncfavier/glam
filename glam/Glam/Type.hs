-- | The basic syntax and operations on types.
module Glam.Type where

import Control.Monad
import Data.List
import Data.String
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Map (Map)
import Data.Map qualified as Map
import Text.Megaparsec
import Control.Monad.Combinators.Expr

import Glam.Utils

-- | Type variables
type TVar = String

-- | Type substitutions
type TSubst = Map TVar Type

-- | Whether a type, or term, is constant. Note that 'False' means "we don't know".
type Constancy = Bool

-- | A fixed point variable can only be used when it is /guarded/ by a @▸@ modality.
data Guardedness = Unguarded -- ^ Can't use it yet
                 | Guarded -- ^ OK, under @▸@
                 | Forbidden -- ^ No way, we're under @■@
                 deriving Eq

infixr 7 :*:
infixr 6 :+:
infixr 5 :->:

-- | Monomorphic types of the guarded λ-calculus
data Type = TVar TVar -- ^ Variables
          | TInt -- ^ Integers
          | TApp Type Type -- ^ Applications
          | One | Type :*: Type -- ^ Products
          | Zero | Type :+: Type -- ^ Sums
          | Type :->: Type -- ^ Functions
          | Later Type -- ^ @▸@ modality
          | Constant Type -- ^ @■@ modality
          | TFix TVar Type -- ^ Fixed points
          deriving Eq

-- | Polymorphic type schemes
data Polytype = Forall [(TVar, Constancy)] Type
              deriving Eq

pattern Monotype ty = Forall [] ty

instance IsString Type where
    fromString = TVar

-- * Variables and substitution

class HasTVars t where
    freeTVars :: t -> Set TVar
    allTVars :: t -> Set TVar

instance HasTVars Type where
    freeTVars (TVar x)     = Set.singleton x
    freeTVars (t1 :*: t2)  = freeTVars t1 <> freeTVars t2
    freeTVars (t1 :+: t2)  = freeTVars t1 <> freeTVars t2
    freeTVars (t1 :->: t2) = freeTVars t1 <> freeTVars t2
    freeTVars (Later t)    = freeTVars t
    freeTVars (Constant t) = freeTVars t
    freeTVars (TFix x t)   = Set.delete x (freeTVars t)
    freeTVars _            = mempty
    allTVars (TVar x)     = Set.singleton x
    allTVars (t1 :*: t2)  = allTVars t1 <> allTVars t2
    allTVars (t1 :+: t2)  = allTVars t1 <> allTVars t2
    allTVars (t1 :->: t2) = allTVars t1 <> allTVars t2
    allTVars (Later t)    = allTVars t
    allTVars (Constant t) = allTVars t
    allTVars (TFix x t)   = Set.insert x (allTVars t)
    allTVars _            = mempty

instance HasTVars Polytype where
    freeTVars (Forall (map fst -> xs) ty) = freeTVars ty Set.\\ Set.fromList xs
    allTVars (Forall (map fst -> xs) ty) = allTVars ty <> Set.fromList xs

x `freeInType` t = x `Set.member` freeTVars t

freshTVarsFor :: Set TVar -> [TVar]
freshTVarsFor xs = [x | n <- [1..]
                      , x <- replicateM n ['a'..'z']
                      , x `Set.notMember` xs]

avoidCaptureType vs (x, ty)
    | x `Set.member` vs = (y, substituteType1 x (TVar y) ty)
    | otherwise = (x, ty)
    where y:_ = freshTVarsFor (vs <> Set.delete x (freeTVars ty))

substituteType :: TSubst -> Type -> Type
substituteType s (TVar x) = Map.findWithDefault (TVar x) x s
substituteType s (t1 :*: t2) = substituteType s t1 :*: substituteType s t2
substituteType s (t1 :+: t2) = substituteType s t1 :+: substituteType s t2
substituteType s (t1 :->: t2) = substituteType s t1 :->: substituteType s t2
substituteType s (Later t1) = Later (substituteType s t1)
substituteType s (Constant t1) = Constant (substituteType s t1)
substituteType s (TFix x tf) = TFix x' (substituteType (Map.delete x' s) tf')
    where (x', tf') = avoidCaptureType (foldMap freeTVars s) (x, tf)
substituteType _ ty = ty

substituteType1 x s = substituteType (Map.singleton x s)

alphaNormalise :: Polytype -> Polytype
alphaNormalise pty@(Forall as ty) = Forall [(b, c) | ((_, c), b) <- s] ty' where
    s = zip as (freshTVarsFor (freeTVars pty))
    ty' = substituteType (Map.fromList [(a, TVar b) | ((a, _), b) <- s]) ty

-- * Printing

prodPrec = 6
sumPrec = 4
funPrec = 0
modPrec = 8
appPrec = 10

instance Show Type where
    showsPrec _ (TVar x)     = showString x
    showsPrec _ TInt         = showString "Int"
    showsPrec d (TApp t1 t2) = showParen (d > appPrec) $
        showsPrec appPrec t1 . showChar ' ' . showsPrec (appPrec + 1) t2
    showsPrec _ One          = showString "1"
    showsPrec d (t1 :*: t2)  = showParen (d > prodPrec) $
        showsPrec (prodPrec + 1) t1 . showString " * " . showsPrec prodPrec t2
    showsPrec _ Zero         = showString "0"
    showsPrec d (t1 :+: t2)  = showParen (d > sumPrec) $
        showsPrec (sumPrec + 1) t1 . showString " + " . showsPrec sumPrec t2
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

-- * Parsing

tVar :: Parser TVar
tVar = mkIdentifier ["type", "Fix", "μ", "Int", "ℤ", "forall"]

tConstant = symbol "#" <|> symbol "■"

type_ :: Parser Type
type_ = tfix <|> makeExprParser base ops <?> "type"
    where
    tfix = TFix <$ ("Fix" <|> "μ") <*> tVar <* dot <*> type_
    base =  TInt <$ ("Int" <|> "ℤ")
        <|> TVar <$> tVar
        <|> One <$ (symbol "1" <|> symbol "⊤")
        <|> Zero <$ (symbol "0" <|> symbol "⊥")
        <|> parens type_
    modality =  Later <$ (symbol ">" <|> symbol "▸")
            <|> Constant <$ tConstant
    ops = [ [InfixL (pure TApp)]
          , [Prefix (foldr1 (.) <$> some modality)]
          , [binary ["*", "×"] (:*:)]
          , [binary ["+"] (:+:)]
          , [binary ["->", "→"] (:->:)] ]
    binary s f = InfixR (f <$ choice (map symbol s))

quantifiedTVar :: Parser (TVar, Constancy)
quantifiedTVar = flip (,) <$> option False (True <$ tConstant) <*> tVar

polytype :: Parser Polytype
polytype = Forall <$> option [] (("forall" <|> symbol "∀") *> some quantifiedTVar <* dot) <*> type_
