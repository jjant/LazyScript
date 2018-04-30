-- TODO: Implement this module.
module Compiler.Parse.Functions where

import Compiler.Parse.Keyword (function_)
import Compiler.Parse.NanoParser (sepBy, varName)
import Compiler.Parse.Primitives.Internals (Parser, char, string)

-- TODO: Change later
data Func = Func
  { identifier :: String
  , formalParameters :: [String]
  , body :: [String]
  } deriving (Show, Eq)

-- Function definitions
-- functionDefinition = do
--
--
{-
  function myFunc(a, b, c) {
    return 3;
  }
-}
-- TODO: This doesn't work correctly atm,
-- it parses "functionMyFunc(a,b,c){}" successfully,
-- so it needs to be modified to correctly handle whitespace. Not sure how to do that atm.
functionDeclaration :: Parser Func
functionDeclaration = do
  function_ -- "function"
  funcName <- varName -- myFunc
  params <- functionParameters -- "(a,b,c)"
  body <- functionBody -- {}
  return (Func {identifier = funcName, formalParameters = params, body = body})

functionParameters :: Parser [String]
functionParameters = do
  char '('
  xs <- varName `sepBy` (char ',')
  char ')'
  return xs

-- TODO: Implement
functionBody :: Parser [String]
functionBody = do
  char '{'
  xs <- return []
  char '}'
  return xs
