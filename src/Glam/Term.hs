module Glam.Term where

import           Control.Monad (replicateM)
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Set (Set)
import qualified Data.Set as Set

import Glam.Parser

type Var = String

type Subst = Map Var Term

infixl :<*>:
infixl 9 :$:

data Term =
    -- Variables
      Var Var
    -- Integers
    | Int Integer | Plus Term Term | Minus Term Term
    -- Products
    | Unit | Pair Term Term | Fst Term | Snd Term
    -- Sums
    | Abort Term | InL Term | InR Term | Case Term Term Term
    -- Functions
    | Abs Var Term | Term :$: Term
    -- Let-bindings
    | Let Delayed
    -- Recursion operations
    | Fold Term | Unfold Term | Fix Term
    -- 'Later' operations
    | Next Term | Prev Term | Term :<*>: Term
    -- 'Constant' operations
    | Box Term | Unbox Term
    deriving Eq

data Delayed = Subst Subst Term deriving Eq

-- Variables

freeVars :: Term -> Set Var
freeVars (Var x)        = Set.singleton x
freeVars (Int _)        = Set.empty
freeVars (Plus t1 t2)   = freeVars t1 <> freeVars t2
freeVars (Minus t1 t2)  = freeVars t1 <> freeVars t2
freeVars Unit           = Set.empty
freeVars (Pair t1 t2)   = freeVars t1 <> freeVars t2
freeVars (Fst t)        = freeVars t
freeVars (Snd t)        = freeVars t
freeVars (Abort t)      = freeVars t
freeVars (InL t)        = freeVars t
freeVars (InR t)        = freeVars t
freeVars (Case t t1 t2) = freeVars t <> freeVars t1 <> freeVars t2
freeVars (Abs x t)      = Set.delete x (freeVars t)
freeVars (t1 :$: t2)    = freeVars t1 <> freeVars t2
freeVars (Let d)        = freeVarsDelayed d
freeVars (Fold t)       = freeVars t
freeVars (Unfold t)     = freeVars t
freeVars (Fix t)        = freeVars t
freeVars (Next t)       = freeVars t
freeVars (Prev t)       = freeVars t
freeVars (t1 :<*>: t2)  = freeVars t1 <> freeVars t2
freeVars (Box t)        = freeVars t
freeVars (Unbox t)      = freeVars t

freeIn :: Var -> Term -> Bool
x `freeIn` t = x `Set.member` freeVars t

freeVarsSubst :: Subst -> Set Var
freeVarsSubst = foldMap freeVars

freeVarsDelayed :: Delayed -> Set Var
freeVarsDelayed (Subst s t) = freeVarsSubst s <> (freeVars t Set.\\ Map.keysSet s)

freshVarFor :: Set Var -> [Var]
freshVarFor vs = [v | n <- [1..]
                    , v <- replicateM n ['a'..'z']
                    , v `Set.notMember` vs]

-- Alpha-renames an abstraction so as to avoid capturing any of the variables in `vs`.
avoidCapture vs t@(Abs x u)
    | x `Set.member` vs = Abs y (substitute1 x (Var y) u)
    | otherwise = t
    where y:_ = freshVarFor (vs <> freeVars t)

-- Same for a delayed substitution.
avoidCaptureDelayed vs d@(Subst s t)
    | not (null conflicts) = Subst s' t'
    | otherwise = d
    where conflicts = Map.keys (Map.restrictKeys s vs)
          renaming = Map.fromList (zip conflicts (freshVarFor (vs <> freeVarsDelayed d <> Map.keysSet s)))
          s' = Map.mapKeys (\v -> Map.findWithDefault v v renaming) s
          t' = substitute (Map.map Var renaming) t

-- Capture-avoiding substitution.
substitute :: Subst -> Term -> Term
substitute s (Var x)        = Map.findWithDefault (Var x) x s
substitute _ (Int i)        = Int i
substitute s (Plus t1 t2)   = Plus (substitute s t1) (substitute s t2)
substitute s (Minus t1 t2)  = Minus (substitute s t1) (substitute s t2)
substitute _ Unit           = Unit
substitute s (Pair t1 t2)   = Pair (substitute s t1) (substitute s t2)
substitute s (Fst t)        = Fst (substitute s t)
substitute s (Snd t)        = Snd (substitute s t)
substitute s (Abort t)      = Abort (substitute s t)
substitute s (InL t)        = InL (substitute s t)
substitute s (InR t)        = InR (substitute s t)
substitute s (Case t t1 t2) = Case (substitute s t) (substitute s t1) (substitute s t2)
substitute s t@Abs{}        = Abs x (substitute (Map.delete x s) u)
    where Abs x u = avoidCapture (freeVarsSubst s) t
substitute s (t1 :$: t2)    = substitute s t1 :$: substitute s t2
substitute s (Let d)        = Let (substituteDelayed s d)
substitute s (Fold t)       = Fold (substitute s t)
substitute s (Unfold t)     = Unfold (substitute s t)
substitute s (Fix t)        = Fix (substitute s t)
substitute s (Next t)       = Next (substitute s t)
substitute s (Prev t)       = Prev (substitute s t)
substitute s (t1 :<*>: t2)  = substitute s t1 :<*>: substitute s t2
substitute s (Box t)        = Box (substitute s t)
substitute s (Unbox t)      = Unbox (substitute s t)

substituteDelayed :: Subst -> Delayed -> Delayed
substituteDelayed s d = Subst (substitute s <$> s') (substitute (s Map.\\ s') t)
    where Subst s' t = avoidCaptureDelayed (freeVarsSubst s) d

substitute1 x s = substitute (Map.singleton x s)

fix_ x t = Fix (Abs x t)

-- Small-step operational semantics for the calculus: performs a single reduction step, if possible.
reduce :: Term -> Maybe Term
-- Reduction rules
reduce (Abs x t :$: s)              = Just (substitute1 x s t)
reduce (Let (Subst s t))            = Just (substitute s t)
reduce (Int a `Plus` Int b)         = Just (Int (a + b))
reduce (Int a `Minus` Int b)        = Just (Int (a - b))
reduce (Fst (Pair t1 _))            = Just t1
reduce (Snd (Pair _ t2))            = Just t2
reduce (Case (InL t) (Abs x1 t1) _) = Just (substitute1 x1 t t1)
reduce (Case (InR t) _ (Abs x2 t2)) = Just (substitute1 x2 t t2)
reduce (Unfold (Fold t))            = Just t
reduce t@(Fix (Abs x t'))           = Just (substitute1 x (Next t) t')
reduce (Next t1 :<*>: Next t2)      = Just (Next (t1 :$: t2))
reduce (Unbox (Box t))              = Just t
reduce (Prev (Next t))              = Just t
-- Context rules (weak call-by-name evaluation)
reduce (t1 :$: t2)    | Just t1' <- reduce t1 = Just (t1' :$: t2)
reduce (Plus t1 t2)   | Just t1' <- reduce t1 = Just (Plus t1' t2)
                      | Just t2' <- reduce t2 = Just (Plus t1 t2')
reduce (Minus t1 t2)  | Just t1' <- reduce t1 = Just (Minus t1' t2)
                      | Just t2' <- reduce t2 = Just (Minus t1 t2')
reduce (Fst t)        | Just t' <- reduce t   = Just (Fst t')
reduce (Snd t)        | Just t' <- reduce t   = Just (Snd t')
reduce (Case t t1 t2) | Just t' <- reduce t   = Just (Case t' t1 t2)
reduce (Unfold t)     | Just t' <- reduce t   = Just (Unfold t')
reduce (Unbox t)      | Just t' <- reduce t   = Just (Unbox t')
reduce (Prev t)       | Just t' <- reduce t   = Just (Prev t')
reduce (t1 :<*>: t2)  | Just t1' <- reduce t1 = Just (t1' :<*>: t2)
                      | Just t2' <- reduce t2 = Just (t1 :<*>: t2')
reduce _ = Nothing

-- The sequence of reduction steps starting with t.
reduction :: Term -> [Term]
reduction t = t:maybe [] reduction (reduce t)

-- Reduce a term to a (weak) normal form.
normalise :: Term -> Term
normalise = last . reduction

-- Printing

showSubst s = intercalate "; " [v ++ " = " ++ show t | (v, t) <- Map.assocs s]

showDelayed kw d (Subst s t) = showParen (d > 0) $
    showString kw . showString " {" . showString (pad (showSubst s)) . showString "} in " . shows t

pad "" = ""
pad s = " " ++ s ++ " "

appPrec = 10
plusPrec = 6

instance Show Term where
    showsPrec _ (Var x) = showString x
    showsPrec _ (Int i) = shows i
    showsPrec d (t1 `Plus` t2) = showParen (d > plusPrec) $
        showsPrec plusPrec t1 . showString " + " . showsPrec (plusPrec + 1) t2
    showsPrec d (t1 `Minus` t2) = showParen (d > plusPrec) $
        showsPrec plusPrec t1 . showString " - " . showsPrec (plusPrec + 1) t2
    showsPrec _ Unit = showString "()"
    showsPrec _ (Pair t1 t2) = showParen True $
        shows t1 . showString ", " . shows t2
    showsPrec d (Fst t) = showParen (d > appPrec) $
        showString "fst " . showsPrec (appPrec + 1) t
    showsPrec d (Snd t) = showParen (d > appPrec) $
        showString "snd " . showsPrec (appPrec + 1) t
    showsPrec d (Abort t) = showParen (d > appPrec) $
        showString "abort " . showsPrec (appPrec + 1) t
    showsPrec d (InL t) = showParen (d > appPrec) $
        showString "left " . showsPrec (appPrec + 1) t
    showsPrec d (InR t) = showParen (d > appPrec) $
        showString "right " . showsPrec (appPrec + 1) t
    showsPrec d (Case t (Abs x1 t1) (Abs x2 t2)) = showParen (d > 0) $
        showString "case " . shows t . showString " of { left "
            . showString x1 . showString ". " . shows t1
            . showString "; right "
            . showString x2 . showString ". " . shows t2
            . showString " }"
    showsPrec d (Abs x t) = showParen (d > 0) $
        showChar '\\' . showString x . showString ". " . shows t
    showsPrec d (t1 :$: t2) = showParen (d > appPrec) $
        showsPrec appPrec t1 . showChar ' ' . showsPrec (appPrec + 1) t2
    showsPrec d (Let d') = showDelayed "let" d d'
    showsPrec d (Fold t) = showParen (d > appPrec) $
        showString "fold " . showsPrec (appPrec + 1) t
    showsPrec d (Unfold t) = showParen (d > appPrec) $
        showString "unfold " . showsPrec (appPrec + 1) t
    showsPrec d (Fix (Abs x t)) = showParen (d > 0) $
        showString "fix " . showString x . showString ". " . shows t
    showsPrec d (Next t) = showParen (d > appPrec) $
        showString "next " . showsPrec (appPrec + 1) t
    showsPrec d (Prev t) = showParen (d > appPrec) $
        showString "prev " . showsPrec (appPrec + 1) t
    showsPrec d (t1 :<*>: t2) = showParen (d > prec) $
        showsPrec prec t1 . showString " <*> " . showsPrec (prec + 1) t2
        where prec = 4
    showsPrec d (Box t) = showParen (d > appPrec) $
        showString "box " . showsPrec (appPrec + 1) t
    showsPrec d (Unbox t) = showParen (d > appPrec) $
        showString "unbox " . showsPrec (appPrec + 1) t

-- Parsing

variable :: Parser Var
variable = mkIdentifier
    ["fst", "snd", "left", "right", "case", "of", "let", "fold", "unfold",
     "fix", "next", "prev", "box", "unbox", "in", "type"]

term :: Parser Term
term = choice [abs_, fix__, case_, letIn, makeExprParser base ops] <?> "term"
    where
    abs_ = flip (foldr Abs) <$ lambda <*> some variable <* dot <*> term
    fix__ = flip (foldr fix_) <$ "fix" <*> some variable <* dot <*> term
    case_ = do
        "case"; t <- term; "of"
        braces do
            "left"; x1 <- variable; dot; t1 <- term
            semicolon
            "right"; x2 <- variable; dot; t2 <- term
            return $ Case t (Abs x1 t1) (Abs x2 t2)
    delayed = Subst <$> braces subst <* "in" <*> term
    letIn = Let <$ "let" <*> delayed
    base =  Var <$> variable
        <|> Int <$> number
        <|> parens (try (Pair <$> term <* comma) <*> term <|> term <|> pure Unit)
    unaries = [("fst", Fst), ("snd", Snd), ("abort", Abort), ("left", InL), ("right", InR),
               ("fold", Fold), ("unfold", Unfold), ("next", Next), ("prev", Prev),
               ("box", Box), ("unbox", Unbox)]
    unary = choice [f <$ hidden (keyword w) | (w, f) <- unaries]
    ops = [ [ InfixL (pure (:$:))
            , Prefix (foldr1 (.) <$> some unary) ]
          , [ InfixL (Plus <$ symbol "+"), InfixL (Minus <$ symbol "-") ]
          , [ InfixL ((:<*>:)        <$ symbol "<*>")
            , InfixL ((:<*>:) . Next <$ symbol "<$>") ] ]

binding :: Parser (Var, Term)
binding = try (mkBinding <$> variable <*> many variable <* equal) <*> term
    where
    mkBinding x ys t = (x, autoFix x (foldr Abs t ys))
    autoFix x t | x `freeIn` t = Fix (Abs x t)
                | otherwise    = t

subst :: Parser Subst
subst = Map.fromList <$> binding `sepBy` semicolon
