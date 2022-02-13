module Parser where

import Control.Applicative

type Token = Char
type Stream = [Token]

newtype Parser e r = Parser { runParser :: Stream -> Either e (Stream, r) }

instance Functor (Parser e) where
    fmap f (Parser p) = Parser $ (fmap . fmap . fmap) f p

instance Applicative (Parser e) where
    pure x = Parser (\s -> Right (s, x))
    Parser f <*> Parser x = Parser g
        where g s = case x s of
                Left e -> Left e
                Right (s, r) -> case f s of
                    Left e -> Left e
                    Right (s', f') -> Right (s', f' r)

instance (Monoid e) => Alternative (Parser e) where
    empty = Parser $ const (Left mempty)
    Parser l <|> Parser r = Parser g
        where g s = case l s of
                Left e -> r s
                Right x -> Right x