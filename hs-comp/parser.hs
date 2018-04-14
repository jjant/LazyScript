module NanoParser where

import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser a = Parser { parse :: String -> [(a, String)] }

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)]   -> error "Parser did not consume entire string"
    _           -> error "Parser error"

item :: Parser Char
item = Parser $ \s ->
  case s of
    []    -> []
    (c:cs)  -> [(c, cs)]

instance Functor Parser where
  fmap f (Parser cs) = Parser $ \s -> [(f a, b) | (a, b) <- cs s]

instance Applicative Parser where
  pure a = Parser $ \s -> [(a, s)]
  Parser pf <*> Parser pa = Parser $ \cs ->
    [(fa a, rest2)| (fa, rest) <- pf cs, (a, rest2) <- pa rest]

instance Monad Parser where
  return = pure
  (Parser p) >>= f = Parser $ \s -> (p s) >>= (\(a, s') -> (parse (f a)) s')

instance Alternative Parser where
  empty = mzero
  Parser p <|> Parser q = Parser $ \cs -> let pcs = p cs in
    if null pcs then q cs else pcs

instance MonadPlus Parser where
  mzero = Parser (const [])
  mplus (Parser p) (Parser q) = Parser $ \cs -> p cs ++ p cs

parser2 :: Parser Int
parser2 = return 2

-- Paramet
data FunctionDecl =
  Function [String] deriving (Eq, Show)

main = do
  putStrLn $ show (parse parser2 "Input")
