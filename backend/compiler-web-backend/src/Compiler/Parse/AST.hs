module Compiler.Parse.AST
  ( JavaScriptValue(..)
  , VarDeclarationType(..)
  , Statement(..)
  , toJS
  ) where

import Data.List (intersperse)

data JavaScriptValue
  = Number Int
  | String String
  | Char Char
  | Bool Bool
  | Object [(String, JavaScriptValue)]
  | Array [JavaScriptValue]
  | ArrowFunc { formalParameters :: [String]
              , body :: [Statement] }
  | Func { identifier :: String
         , formalParameters :: [String]
         , body :: [Statement]
         -- , env :: JavaScriptValue -- Env is an object ?
          }
  | Var String -- TODO: Should this be here?
  deriving (Eq, Show)

data VarDeclarationType
  = VarDecl
  | ConstDecl
  | LetDecl
  deriving (Eq, Show)

data Statement
  = Assignment String
               JavaScriptValue
  | If JavaScriptValue -- Condition
       [Statement] -- Then body
  | IfElse JavaScriptValue --Condition
           [Statement] -- Then body
           [Statement] -- Else body
  | Return JavaScriptValue
  deriving (Eq, Show)

class JSPrintable p where
  toJS :: p -> String

-- TODO: Clean this up.
instance JSPrintable JavaScriptValue where
  toJS (Array xs) = "[" ++ (mconcat . intersperse ", " . (map toJS)) xs ++ "]"
  toJS (ArrowFunc formalParameters body) =
    "(" ++
    (mconcat $ intersperse ", " formalParameters) ++
    ")" ++
    " => " ++
    "{" ++ (unwords . intersperse "\n" . map (" " ++) . map toJS) body ++ "}"
  toJS (Func identifier formalParameters body) =
    "function " ++
    identifier ++
    "(" ++
    (mconcat $ intersperse ", " formalParameters) ++
    ")" ++
    " {\n" ++
    (unwords . intersperse "\n" . map (" " ++) . map toJS) body ++ "\n}"
  toJS (Number x) = show x
  toJS (String x) = show x
  toJS (Bool True) = "true"
  toJS (Bool False) = "false"
  toJS (Var b) = b
  toJS _ = error "not yet implemented"

instance JSPrintable Statement where
  toJS (Assignment varName val) = varName ++ " = " ++ "val"
  toJS (Return val) = "return " ++ toJS val ++ ";"
  toJS _ = error "not yet implemented"
