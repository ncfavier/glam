{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Glam.Parse where

import           Data.Void
import           Data.Bifunctor (first)
import qualified Data.Map as Map (fromList)
import           Data.String
import           Control.Monad.Combinators.Expr
import           Control.Monad.State
import           Numeric.Natural
import           Text.Megaparsec hiding (State)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Glam.Term
import Glam.Type

type IndentState = Maybe SourcePos

type Parser = ParsecT Void String (State IndentState)

runP :: Parser a -> String -> String -> Either String a
runP p f s = first errorBundlePretty $ evalState (runParserT p f s) Nothing

instance {-# OVERLAPPING #-} a ~ String => IsString (Parser a) where
    fromString = keyword

sp = L.space ((() <$) $ satisfy $ \c -> c == ' ' || c == '\t')
             (L.skipLineComment "--")
             (L.skipBlockComment "{-" "-}")
spn = L.space space1
              (L.skipLineComment "--")
              (L.skipBlockComment "{-" "-}")
lexeme = L.lexeme sp
symbol = L.symbol sp

parens    = between (symbol "(") (symbol ")")
braces    = between (symbol "{") (symbol "}")
semicolon = symbol ";"
comma     = symbol ","
equal     = symbol "="
dot       = symbol "."

alpha = letterChar <|> char '_'

digit = digitChar

word = (:) <$> alpha <*> hidden (many (alpha <|> digit <|> char '\'')) <?> "word"

number = lexeme L.decimal

keyword s = label (show s) $ try $ lexeme $ string s <* notFollowedBy alphaNumChar

reserved = ["succ", "fst", "snd", "abort", "left", "right", "case", "of", "let",
            "fold", "unfold", "fix", "next", "prev", "box", "unbox", "in"]

ident = label "identifier" $ try $ lexeme $ do
    w <- word
    if w `elem` reserved
        then fail $ "unexpected keyword " ++ w
        else return w

lambda = symbol "λ" <|> symbol "\\"

term :: Parser Term
term = choice [abs_, fix__, case_, letIn, try prevIn, try boxIn, makeExprParser base ops] <?> "term"
    where
    abs_ = flip (foldr Abs) <$ lambda <*> some ident <* dot <*> term
    fix__ = flip (foldr fix_) <$ "fix" <*> some ident <* dot <*> term
    case_ = do
        "case"; t <- term; "of"
        braces $ do
            "left"; x1 <- ident; dot; t1 <- term
            semicolon
            "right"; x2 <- ident; dot; t2 <- term
            return $ Case t (Abs x1 t1) (Abs x2 t2)
    delayed = Subst <$> braces subst <* "in" <*> term
    letIn = Let <$> ("let" *> delayed)
    prevIn = Prev <$> ("prev" *> delayed)
    boxIn = Box <$> ("box" *> delayed)
    base = choice [Var <$> ident, natToTerm <$> number, mkParens =<< parens (term `sepBy` comma)]
    mkParens []       = pure Unit
    mkParens [t]      = pure t
    mkParens [t1, t2] = pure (Pair t1 t2)
    mkParens _        = fail "too many items in tuple"
    unaries = [("succ", Succ), ("fst", Fst), ("snd", Snd), ("abort", Abort), ("left", InL), ("right", InR),
               ("fold", Fold), ("unfold", Unfold), ("next", Next), ("prev", prev), ("box", box), ("unbox", Unbox)]
    ops = [ InfixL (pure (:$:)) : map unary unaries
          , [InfixL ((:<*>:) <$ symbol "<*>"), InfixL ((:<*>:) . Next <$ symbol "<$>")] ]
    unary (w, f) = Prefix (f <$ hidden (keyword w))

def :: Parser (Var, Term)
def = try (mkDef <$> ident <*> many ident <* equal) <*> term
    where
    mkDef x ys t = autoFix x (foldr Abs t ys)
    autoFix x t | x `freeIn` t = (x, Fix (Abs x t))
                | otherwise = (x, t)

subst :: Parser Subst
subst = Map.fromList <$> def `sepBy` semicolon

typeIdent = label "identifier" $ try $ lexeme $ do
    w <- word
    if w == "Fix"
        then fail $ "unexpected keyword " ++ w
        else return w

type_ :: Parser Type
type_ = tfix <|> makeExprParser base ops <?> "type"
    where
    tfix = flip (foldr TFix) <$ "Fix" <*> some typeIdent <* dot <*> type_
    base = TVar <$> typeIdent <|> One <$ symbol "1" <|> parens type_
    mod_ = Later <$ symbol ">" <|> Constant <$ symbol "#"
    ops = [ [Prefix (foldr1 (.) <$> some mod_)]
          , [binary "*" (:*:)]
          , [binary "+" (:+:)]
          , [binary "->" (:->:)] ]
    binary w f = InfixR (f <$ symbol w)

data Statement = Define Var Term | Print Term
    deriving Eq

statement :: Parser Statement
statement = uncurry Define <$> def <|> Print <$> term

file :: Parser [Statement]
file = spn *> statement `sepEndBy` spn <* eof
