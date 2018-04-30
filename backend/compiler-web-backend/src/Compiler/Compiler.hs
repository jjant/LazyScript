module Compiler.Compiler where

import qualified Compiler.Parse.NanoParser as P

-- compiler :: String
compiler = P.runModule
