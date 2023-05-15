module Glam.Term where

import Data.Function
import Data.List
import Data.Map.Lazy (Map)
import Data.Map.Lazy qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

import Glam.Utils

type Var = String

type Subst = Map Var Term

infixl :<*>:
infixl 9 :$:

data Term =
    -- Variables
      Var Var
    -- Integers
    | Int Integer | Plus Term Term | Minus Term Term | Times Term Term | Divide Term Term | IntRec
    -- Products
    | Unit | Pair Term Term | Fst Term | Snd Term
    -- Sums
    | Abort Term | InL Term | InR Term | Case Term Term Term
    -- Functions
    | Abs Var Term | Term :$: Term
    -- Let-bindings
    | Let Subst Term
    -- Recursion operations
    | Fold Term | Unfold Term | Fix Term
    -- 'Later' operations
    | Next Term | Prev Term | Term :<*>: Term
    -- 'Constant' operations
    | Box Term | Unbox Term
    deriving Eq

-- Variables

freeVars :: Term -> Set Var
freeVars (Var x)        = Set.singleton x
freeVars (Int _)        = Set.empty
freeVars (Plus t1 t2)   = freeVars t1 <> freeVars t2
freeVars (Minus t1 t2)  = freeVars t1 <> freeVars t2
freeVars (Times t1 t2)  = freeVars t1 <> freeVars t2
freeVars (Divide t1 t2) = freeVars t1 <> freeVars t2
freeVars IntRec         = Set.empty
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
freeVars (Let s t)      = foldMap freeVars s <> (freeVars t Set.\\ Map.keysSet s)
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

-- Evaluation

data Value = VInt !Integer
           | VUnit | VPair Value Value
           | VInL Value | VInR Value
           | VAbs (Value -> Value)
           | VFold Value
           | VNext Value
           | VBox Value

intrec :: (a -> a) -> a -> (a -> a) -> Integer -> a
intrec p z s = go where
    go n = case compare n 0 of
        LT -> p (go (succ n))
        EQ -> z
        GT -> s (go (pred n))

eval :: Map Var Value -> Term -> Value
eval s (Var x)          = s Map.! x
eval _ (Int i)          = VInt i
eval s (Plus t1 t2)     = case (eval s t1, eval s t2) of ~(VInt i1, VInt i2) -> VInt (i1 + i2)
eval s (Minus t1 t2)    = case (eval s t1, eval s t2) of ~(VInt i1, VInt i2) -> VInt (i1 - i2)
eval s (Times t1 t2)    = case (eval s t1, eval s t2) of ~(VInt i1, VInt i2) -> VInt (i1 * i2)
eval s (Divide t1 t2)   = case (eval s t1, eval s t2) of ~(VInt i1, VInt i2) -> VInt (i1 `div` i2)
eval _ IntRec           = VAbs \(VAbs p) -> VAbs \z -> VAbs \(VAbs s) -> VAbs \(VInt n) -> intrec p z s n
eval _ Unit             = VUnit
eval s (Pair t1 t2)     = VPair (eval s t1) (eval s t2)
eval s (Fst t)          = case eval s t of ~(VPair t1 _) -> t1
eval s (Snd t)          = case eval s t of ~(VPair _ t2) -> t2
eval _ (Abort _)        = undefined
eval s (InL t)          = VInL (eval s t)
eval s (InR t)          = VInR (eval s t)
eval s (Case t ~(Abs x1 t1) ~(Abs x2 t2)) = case eval s t of
    VInL l -> eval (Map.insert x1 l s) t1
    VInR r -> eval (Map.insert x2 r s) t2
    _ -> undefined
eval s (Abs x t)        = VAbs (\ v -> eval (Map.insert x v s) t)
eval s (t1 :$: t2)      = case eval s t1 of ~(VAbs f) -> f (eval s t2)
eval s (Let s' t)       = eval (Map.union (eval s <$> s') s) t
eval s (Fold t)         = VFold (eval s t)
eval s (Unfold t)       = case eval s t of ~(VFold t) -> t
eval s (Fix ~(Abs x t)) = fix \ self -> eval (Map.insert x (VNext self) s) t
eval s (Next t)         = VNext (eval s t)
eval s (Prev t)         = case eval s t of ~(VNext t) -> t
eval s (t1 :<*>: t2)    = case (eval s t1, eval s t2) of ~(VNext (VAbs f), VNext t2) -> VNext (f t2)
eval s (Box t)          = VBox (eval s t)
eval s (Unbox t)        = case eval s t of ~(VBox t) -> t

-- Printing

showSubst s = intercalate "; " [v ++ " = " ++ show t | (v, t) <- Map.assocs s]

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
    showsPrec d (t1 `Times` t2) = showParen (d > plusPrec) $
        showsPrec plusPrec t1 . showString " * " . showsPrec (plusPrec + 1) t2
    showsPrec d (t1 `Divide` t2) = showParen (d > plusPrec) $
        showsPrec plusPrec t1 . showString " / " . showsPrec (plusPrec + 1) t2
    showsPrec _ IntRec = showString "intrec"
    showsPrec _ Unit = showString "()"
    showsPrec d (Pair t1 t2) = showParen (d >= 0) $
        shows t1 . showString ", " . showsPrec (-1) t2
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
    showsPrec d (Case t ~(Abs x1 t1) ~(Abs x2 t2)) = showParen (d > 0) $
        showString "case " . shows t . showString " of { left "
            . showString x1 . showString ". " . shows t1
            . showString "; right "
            . showString x2 . showString ". " . shows t2
            . showString " }"
    showsPrec d (Abs x t) = showParen (d > 0) $
        showChar '\\' . showString x . showString ". " . shows t
    showsPrec d (t1 :$: t2) = showParen (d > appPrec) $
        showsPrec appPrec t1 . showChar ' ' . showsPrec (appPrec + 1) t2
    showsPrec d (Let s t) = showParen (d > 0) $
        showString "let {" . showString (pad (showSubst s)) . showString "} in " . shows t
    showsPrec d (Fold t) = showParen (d > appPrec) $
        showString "fold " . showsPrec (appPrec + 1) t
    showsPrec d (Unfold t) = showParen (d > appPrec) $
        showString "unfold " . showsPrec (appPrec + 1) t
    showsPrec d (Fix ~(Abs x t)) = showParen (d > 0) $
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

instance Show Value where
    showsPrec _ (VInt i) = shows i
    showsPrec _ VUnit = showString "()"
    showsPrec d (VPair t1 t2) = showParen (d >= 0) $
        shows t1 . showString ", " . showsPrec (-1) t2
    showsPrec d (VInL t) = showParen (d > appPrec) $
        showString "left " . showsPrec (appPrec + 1) t
    showsPrec d (VInR t) = showParen (d > appPrec) $
        showString "right " . showsPrec (appPrec + 1) t
    showsPrec _ (VAbs _) = showString "<function>"
    showsPrec d (VFold t) = showParen (d > appPrec) $
        showString "fold " . showsPrec (appPrec + 1) t
    showsPrec d (VNext t) = showParen (d > appPrec) $
        showString "next " . showsPrec (appPrec + 1) t
    showsPrec d (VBox t) = showParen (d > appPrec) $
        showString "box " . showsPrec (appPrec + 1) t

-- Parsing

variable :: Parser Var
variable = mkIdentifier
    ["intrec", "fst", "snd", "left", "right", "case", "of", "let", "fold", "unfold",
     "fix", "next", "prev", "box", "unbox", "in", "type"]

term :: Parser Term
term = choice [abs_, fix_, case_, letIn, makeExprParser base ops] <?> "term"
    where
    abs_ = flip (foldr Abs) <$ lambda <*> some variable <* dot <*> term
    fix_ = Fix <$> (flip (foldr Abs) <$ "fix" <*> some variable <* dot <*> term)
    case_ = do
        "case"; t <- term; "of"
        braces do
            "left"; x1 <- variable; dot; t1 <- term
            semicolon
            "right"; x2 <- variable; dot; t2 <- term
            pure $ Case t (Abs x1 t1) (Abs x2 t2)
    letIn = Let <$ "let" <*> braces subst <* "in" <*> term
    base =  Var <$> variable
        <|> Int <$> number
        <|> IntRec <$ "intrec"
        <|> parens (tuple <$> term `sepBy` comma)
    tuple [] = Unit
    tuple [t] = t
    tuple (t : ts) = Pair t (tuple ts)
    unaries = [("fst", Fst), ("snd", Snd), ("abort", Abort), ("left", InL), ("right", InR),
               ("fold", Fold), ("unfold", Unfold), ("next", Next), ("prev", Prev),
               ("box", Box), ("unbox", Unbox)]
    unary = choice [f <$ hidden (keyword w) | (w, f) <- unaries]
    ops = [ [ InfixL (pure (:$:))
            , Prefix (foldr1 (.) <$> some unary) ]
          , [ InfixL (Plus <$ symbol "+"), InfixL (Minus <$ symbol "-")
            , InfixL (Times <$ symbol "*"), InfixL (Divide <$ symbol "/") ]
          , [ InfixL ((:<*>:)        <$ (symbol "<*>" <|> symbol "⊛"))
            , InfixL ((:<*>:) . Next <$ symbol "<$>") ] ]

binding :: Parser (Var, Term)
binding = try (mkBinding <$> variable <*> many variable <* equal) <*> term
    where
    mkBinding x ys t = (x, autoFix x (foldr Abs t ys))
    autoFix x t | x `freeIn` t = Fix (Abs x t)
                | otherwise    = t

subst :: Parser Subst
subst = Map.fromList <$> binding `sepBy` semicolon
