module NanoParser where

import Data.Char
import Control.Monad
import Control.Applicative

newtype Parser a = Parser { parse :: String -> [(a, String)] }

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

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)]   -> error "Parser did not consume entire string"
    []          -> error "No matches"
    _           -> error "Parser error"

item :: Parser Char
item = Parser $ \s ->
  case s of
    []    -> []
    (c:cs)  -> [(c, cs)]

sat :: (Char -> Bool) -> Parser Char
sat p = item >>= (\c -> if p c then return c else mzero)

char :: Char -> Parser Char
char c = sat (== c)

string :: [Char] -> Parser [Char]
string [] = pure []
string (x:xs) = do
  char x
  string xs
  return $ x:xs

parseArrow = string "=>"

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (p `sepBy1` sep) <|> pure []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

alpha :: Parser Char
alpha = foldl (<|>) mzero (char <$> ['a'..'z']++['A'..'Z'])

digit :: Parser Char
digit = foldl (<|>) mzero (char <$>['0'..'9'])

alphaNum :: Parser Char
alphaNum = digit <|> alpha

varName :: Parser [Char]
varName = many alpha

parseNumber :: Parser JsValue
parseNumber = (JsNumber . read <$> some digit)

parseString :: Parser JsValue
parseString = do
  char '"'
  a <- many alphaNum
  char '"'
  return (JsString a)

parseArray :: Parser JsValue
parseArray = do
  char '['
  elems <- parseJsExpression `sepBy` char ','
  char ']'
  return (JsArray elems)

parseObject :: Parser JsValue
parseObject = do
  char '{'
  kvs <- many keyValuePair
  char '}'
  return (JsObject kvs)

keyValuePair :: Parser (String, JsValue)
keyValuePair = do
  k <- varName
  char ':'
  v <- parseJsValue
  return (k,v)

parseJsValue :: Parser JsValue
parseJsValue =
  parseObject <|>
  parseArray <|>
  parseNumber <|>
  parseString

spread :: Parser JsExpression
spread = Spread <$> (string "..." >> parseJsExpression)

parseFuncCall :: Parser JsExpression
parseFuncCall = do
  funcName <- varName
  char '('
  params <- parseJsValue `sepBy` (char ',')
  char ')'
  return $ JsFuncCall funcName params

parseJsExpression :: Parser JsExpression
parseJsExpression =
  (Value <$> parseJsValue) <|>
  spread <|>
  parseFuncCall

data JsValue =
  JsNumber Int
  | JsString String
  | JsObject [(String, JsValue)]
  | JsArray [JsExpression]
  deriving (Show)

data JsExpression =
  Value JsValue
  | Spread JsExpression
  | JsFuncCall String [JsValue]
  deriving (Show)


objString :: String
objString = "{hola:{}}"

arrayString :: String
arrayString = "[...a(),1,2,3]"

main = do
  putStrLn $ show (runParser parseJsExpression objString)
  putStrLn $ show (runParser parseArray arrayString)
