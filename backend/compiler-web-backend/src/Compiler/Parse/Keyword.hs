-- TODO: Implement this module
module Compiler.Parse.Keyword where

import Compiler.Parse.Primitives.Internals (Parser, string)

await_ :: Parser ()
await_ = string "await" >> return ()

break_ :: Parser ()
break_ = string "break" >> return ()

case_ :: Parser ()
case_ = string "case" >> return ()

catch_ :: Parser ()
catch_ = string "catch" >> return ()

class_ :: Parser ()
class_ = string "class" >> return ()

const_ :: Parser ()
const_ = string "const" >> return ()

continue_ :: Parser ()
continue_ = string "continue" >> return ()

debugger_ :: Parser ()
debugger_ = string "debugger" >> return ()

default_ :: Parser ()
default_ = string "default" >> return ()

delete_ :: Parser ()
delete_ = string "delete" >> return ()

do_ :: Parser ()
do_ = string "do" >> return ()

else_ :: Parser ()
else_ = string "else" >> return ()

export_ :: Parser ()
export_ = string "export" >> return ()

extends_ :: Parser ()
extends_ = string "extends" >> return ()

finally_ :: Parser ()
finally_ = string "finally" >> return ()

for_ :: Parser ()
for_ = string "for" >> return ()

function_ :: Parser ()
function_ = string "function" >> return ()

if_ :: Parser ()
if_ = string "if" >> return ()

import_ :: Parser ()
import_ = string "import" >> return ()

in_ :: Parser ()
in_ = string "in" >> return ()

instanceof_ :: Parser ()
instanceof_ = string "instanceof" >> return ()

new_ :: Parser ()
new_ = string "new" >> return ()

return_ :: Parser ()
return_ = string "return" >> return ()

super_ :: Parser ()
super_ = string "super" >> return ()

switch_ :: Parser ()
switch_ = string "switch" >> return ()

this_ :: Parser ()
this_ = string "this" >> return ()

throw_ :: Parser ()
throw_ = string "throw" >> return ()

try_ :: Parser ()
try_ = string "try" >> return ()

typeof_ :: Parser ()
typeof_ = string "typeof" >> return ()

var_ :: Parser ()
var_ = string "var" >> return ()

void_ :: Parser ()
void_ = string "void" >> return ()

while_ :: Parser ()
while_ = string "while" >> return ()

with_ :: Parser ()
with_ = string "with" >> return ()

yield_ :: Parser ()
yield_ = string "yield" >> return ()

-- BOOLEANS
true_ :: Parser ()
true_ = string "true" >> return ()

false_ :: Parser ()
false_ = string "false" >> return ()

-- NULL
null_ :: Parser ()
null_ = string "null" >> return ()

-- UNDEFINED
undefined_ :: Parser ()
undefined_ = string "undefined" >> return ()

main = putStrLn "Loaded"
