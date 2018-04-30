-- TODO: This module needs to be completely overhauled (js parsing)
module Compiler.Parse.NanoParser where

import Compiler.Parse.Primitives.Internals
import Control.Applicative
import Control.Monad
import Data.Char
import Data.Monoid
import Data.Text (Text)

parseArrow = string "=>"

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (p `sepBy1` sep) <|> pure []

sepBy1 :: Parser a -> Parser b -> Parser [a]
sepBy1 p sep = (:) <$> p <*> many (sep *> p)

alpha :: Parser Char
alpha = foldl (<|>) mzero (char <$> ['a' .. 'z'] ++ ['A' .. 'Z'])

digit :: Parser Char
digit = foldl (<|>) mzero (char <$> ['0' .. '9'])

alphaNum :: Parser Char
alphaNum = digit <|> alpha

varName :: Parser [Char]
varName = some alpha

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
  return (k, v)

parseJsValue :: Parser JsValue
parseJsValue = parseObject <|> parseArray <|> parseNumber <|> parseString

spread :: Parser JsExpression
spread = Spread <$> (string "..." >> parseJsExpression)

parseFunc :: Parser JsExpression
parseFunc = do
  char '('
  paramNames <- varName `sepBy` (char ',')
  char ')'
  string "=>"
  char '{'
  exps <- parseJsExpression `sepBy` (char '\n')
  char '}'
  return $ JsFunc paramNames exps

parseFuncCall :: Parser JsExpression
parseFuncCall = do
  funcName <- varName
  char '('
  params <- parseJsValue `sepBy` (char ',')
  char ')'
  return $ JsFuncCall funcName params

parseJsExpression :: Parser JsExpression
parseJsExpression = (Value <$> parseJsValue) <|> spread <|> parseFuncCall

data JsValue
  = JsNumber Int
  | JsString String
  | JsObject [(String, JsValue)]
  | JsArray [JsExpression]
  deriving (Show)

data JsExpression
  = Value JsValue
  | Spread JsExpression
  | JsFuncCall String
               [JsValue]
  | JsFunc [String]
           [JsExpression]
  deriving (Show)

data JavaScriptValue
  = Number Int
  | String String
  | Char Char
  | Bool Bool
  | Object [(String, JavaScriptValue)]
  | Array [JavaScriptValue]

objString :: String
objString = "{hola:{}}"

arrayString :: String
arrayString = "[...a(),1,2,3]"

funcString :: String
funcString = "(a)=>{2}"
-- runModule :: Text -> Text
-- runModule s =
--   s <> "\n" <> show (runParser parseJsExpression objString) <>
--   show (runParser parseArray arrayString) <>
--   show (runParser parseFunc funcString)
