module Compiler.Lazify.LazyArray
  ( lazifyArray
  , main
  ) where

import Compiler.Parse.AST (JavaScriptValue(..), Statement(Return), toJS)

lazifyArray :: JavaScriptValue -> JavaScriptValue
lazifyArray (Array []) = toThunk (Array [])
lazifyArray (Array (x:xs)) = toThunk (Array [x, (lazifyArray (Array xs))])
lazifyArray jsVal = jsVal -- Only care about arrays.

toThunk :: JavaScriptValue -> JavaScriptValue
toThunk jsVal = ArrowFunc {formalParameters = [], body = [Return jsVal]}

-- lazifyArray myArray
-- Should result in the following thunk:
-- () => [
-- 	4,
-- 	() => [
-- 	  true,
-- 		() => [
-- 			myArray,
-- 			() => []
-- 		]
--   ]
-- ]
myArray = Array [Number 4, Bool True, Var "myArray"]

printJS :: JavaScriptValue -> IO ()
printJS = putStrLn . toJS

main = do
  printJS myArray
  printJS $ lazifyArray myArray
