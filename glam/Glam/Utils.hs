{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
module Glam.Utils (
    module Glam.Utils,
    module Text.Megaparsec,
    module Control.Monad.Combinators.Expr
) where

import Control.Monad.Combinators.Expr
import Control.Monad.Reader
import Data.Bifunctor (first)
import Data.Char
import Data.String
import Data.Void
import Text.Megaparsec hiding (State, parse)
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as L

-- Parsing

type IndentRef = Maybe SourcePos

type Parser = ReaderT IndentRef (Parsec Void String)

parse :: Parser a -> String -> String -> Either String a
parse p f s = first (init . errorBundlePretty) $ runParser (runReaderT p Nothing) f s

whitespace :: Parser ()
whitespace = L.space space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

alpha :: Parser Char
alpha = letterChar <|> char '_'

isRest :: Char -> Bool
isRest c = c == '\'' || c == '_' || isDigit c || isAlpha c

lexeme :: Parser a -> Parser a
lexeme p = do
    SourcePos { sourceLine = curLine, sourceColumn = curColumn } <- getSourcePos
    ref <- ask
    case ref of
        Just SourcePos { sourceLine = refLine, sourceColumn = refColumn }
            | curLine > refLine, curColumn <= refColumn ->
                L.incorrectIndent GT refColumn curColumn
        _ -> pure ()
    p <* whitespace

symbol, keyword :: String -> Parser String
symbol  s = lexeme (string s)
keyword s = label (show s) $ try $ lexeme $ string s <* notFollowedBy (satisfy isRest)

instance {-# OVERLAPPING #-} a ~ String => IsString (Parser a) where
    fromString = keyword

colon, semicolon, comma, equal, dot, lambda :: Parser String
colon     = symbol ":"
semicolon = symbol ";"
comma     = symbol ","
equal     = symbol "="
dot       = symbol "."
lambda    = symbol "λ" <|> symbol "\\"

parens, braces, lineFolded :: Parser a -> Parser a
parens       = between (symbol "(") (symbol ")")
braces       = between (symbol "{") (symbol "}")
lineFolded p = do
    pos <- getSourcePos
    local (\_ -> Just pos) p

word :: Parser String
word = (:) <$> alpha <*> takeWhileP Nothing isRest <?> "word"

number :: Parser Integer
number = lexeme L.decimal

mkIdentifier :: [String] -> Parser String
mkIdentifier reserved = label "identifier" $ try $ lexeme do
    w <- word
    if w `elem` reserved
        then fail $ "unexpected keyword " ++ w
        else pure w

-- Type checking

infix 1 |-
(|-) = local
