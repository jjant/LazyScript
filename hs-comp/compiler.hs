import Data.List

data Expression
  = Literal Int
  | Identifier String
  | ArrayExpression [Expression]
  | FuncCall String
  | Spread String
  | NoOp
  | Thunk Expression
  deriving (Eq, Show)

data Declaration =
  Const String
        Expression
  deriving (Eq, Show)

-- const a = [1,2,3]
anArray :: Expression
anArray = (ArrayExpression [Literal 1, Literal 2, Literal 3, Spread "a"])

aDeclaration :: Declaration
aDeclaration = Const ("a") anArray

makeLazy :: Expression -> Expression
makeLazy (Spread exp) = FuncCall exp
-- makeLazy (ArrayExpression []) = Thunk (ArrayExpression [x])
makeLazy (ArrayExpression (Spread a:_)) = FuncCall a
makeLazy (ArrayExpression (x:xs)) =
  ArrayExpression ([makeLazy x, Thunk (makeLazy (ArrayExpression xs))])
makeLazy e = e

toJS :: Expression -> String
toJS (ArrayExpression []) = "[]"
toJS (ArrayExpression xs) =
  "[" ++ (unwords . intersperse "," . map toJS) xs ++ "]"
toJS (FuncCall f) = f ++ "()"
toJS (Spread id) = "..." ++ id
toJS (Literal lit) = show lit
toJS (Identifier ident) = ident
toJS (Thunk exp) = "() => { return " ++ (toJS exp) ++ "; }"
toJS (NoOp) = ""

declToJS :: Declaration -> String
declToJS (Const s exp) = "const " ++ s ++ " = " ++ (toJS exp)

accessFunc :: String
accessFunc =
  "const access = (n, ari) => (n === 0 ? ari()[0] : access(n - 1, ari()[1]));"

testCases :: String
testCases =
  "console.log(access(10, a).toString() + ' === 2');\nconsole.log(access(0, a).toString() + ' === 1');"

main
  -- putStrLn ("Array "  ++ (show anArray))
  -- putStrLn ("Lazy " ++ (show (makeLazy anArray)))
  -- putStrLn $ "const b = " ++ toJS anArray
 = do
  putStrLn $ "const a = () => " ++ (toJS . makeLazy) anArray
  putStrLn $ accessFunc
  putStrLn $ testCases
