{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Glam.Parse (
    module Glam.Parse,
    module Text.Megaparsec,
    module Control.Monad.Combinators.Expr
) where

import           Control.Monad.Combinators.Expr
import           Control.Monad.State
import           Data.Bifunctor (first)
import qualified Data.Map as Map (fromList)
import           Data.Char
import           Data.String
import           Data.Void
import           Numeric.Natural
import           Text.Megaparsec hiding (State, parse)
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type IndentState = Maybe SourcePos

type Parser = StateT IndentState (Parsec Void String)

parse :: Parser a -> String -> String -> Either String a
parse p f s = first errorBundlePretty $ runParser (evalStateT p Nothing) f s

whitespace :: Parser ()
whitespace = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

alpha :: Parser Char
alpha = letterChar <|> char '_'

isRest :: Char -> Bool
isRest c = c == '\'' || isDigit c || isAlpha c

lexeme :: Parser a -> Parser a
lexeme p = do
    SourcePos { sourceLine = curLine, sourceColumn = curColumn } <- getSourcePos
    ref <- get
    case ref of
        Just SourcePos { sourceLine = refLine, sourceColumn = refColumn }
            | curLine > refLine, curColumn <= refColumn ->
                L.incorrectIndent GT refColumn curColumn
        _ -> return ()
    p <* whitespace

symbol, keyword :: String -> Parser String
symbol  s = lexeme (string s)
keyword s = label (show s) $ try $ lexeme $ string s <* notFollowedBy (satisfy isRest)

instance {-# OVERLAPPING #-} a ~ String => IsString (Parser a) where
    fromString = keyword

semicolon, comma, equal, dot, lambda :: Parser String
semicolon = symbol ";"
comma     = symbol ","
equal     = symbol "="
dot       = symbol "."
lambda    = symbol "λ" <|> symbol "\\"

parens, braces, lineFolded :: Parser a -> Parser a
parens       = between (symbol "(") (symbol ")")
braces       = between (symbol "{") (symbol "}")
lineFolded p = (put . Just =<< getSourcePos) *> p

word :: Parser String
word = (:) <$> alpha <*> takeWhileP Nothing isRest <?> "word"

number :: Parser Natural
number = lexeme L.decimal

mkIdentifier :: [String] -> Parser String
mkIdentifier reserved = label "identifier" $ try $ lexeme $ do
    w <- word
    if w `elem` reserved
        then fail $ "unexpected keyword " ++ w
        else return w
