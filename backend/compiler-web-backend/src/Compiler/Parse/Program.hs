-- TODO: Implement this module.
-- Sources:
-- https://www-archive.mozilla.org/js/language/grammar14.html
-- Page 233 onwards: http://www.ecma-international.org/publications/files/ECMA-ST/Ecma-262.pdf
module Compiler.Parse.Program where

import Compiler.Parse.Primitives.Internals (Parser, string)

emptyProgram :: Parser ()
emptyProgram = string "" >> return ()

program :: Parser ()
program = many topStatement

topStatement = statement <|> functionDefinition
