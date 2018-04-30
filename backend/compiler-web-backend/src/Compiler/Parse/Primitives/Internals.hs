module Compiler.Parse.Primitives.Internals
  ( Parser(..)
  , runParser
  , item
  , char
  , string
  , sat
  ) where

import Control.Applicative
import Control.Monad

newtype Parser a = Parser
  { parse :: String -> [(a, String)]
  }

instance Functor Parser where
  fmap f (Parser cs) = Parser $ \s -> [(f a, b) | (a, b) <- cs s]

instance Applicative Parser where
  pure a = Parser $ \s -> [(a, s)]
  Parser pf <*> Parser pa =
    Parser $ \cs -> [(fa a, rest2) | (fa, rest) <- pf cs, (a, rest2) <- pa rest]

instance Monad Parser where
  return = pure
  (Parser p) >>= f = Parser $ \s -> (p s) >>= (\(a, s') -> (parse (f a)) s')

instance Alternative Parser where
  empty = mzero
  Parser p <|> Parser q =
    Parser $ \cs ->
      let pcs = p cs
      in if null pcs
           then q cs
           else pcs

instance MonadPlus Parser where
  mzero = Parser (const [])
  mplus (Parser p) (Parser q) = Parser $ \cs -> p cs ++ q cs

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, _)] -> error "Parser did not consume entire string"
    [] -> error "No matches"
    _ -> error "Parser error"

item :: Parser Char
item =
  Parser $ \s ->
    case s of
      [] -> []
      (c:cs) -> [(c, cs)]

sat :: (Char -> Bool) -> Parser Char
sat p =
  item >>=
  (\c ->
     if p c
       then return c
       else mzero)

char :: Char -> Parser Char
char c = sat (== c)

string :: [Char] -> Parser [Char]
string [] = pure []
string (x:xs) = char x >> string xs >> return (x : xs)
