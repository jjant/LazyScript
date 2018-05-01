module Compiler.Compiler where

import qualified Compiler.Parse.Parser as P

-- compiler :: String
compiler = P.runModule
