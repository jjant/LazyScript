-- TODO: Implement this module.
-- Sources:
-- https://www-archive.mozilla.org/js/language/grammar14.html
-- Page 233 onwards: http://www.ecma-international.org/publications/files/ECMA-ST/Ecma-262.pdf
module Compiler.Parse.Operators where

import Compiler.Parse.Primitives.Internals (Parser, string)

-- POSTFIX OPERATORS
-- a++
plusplus_ = string "++"

-- a--
minusminus_ = string "--"

-- Unary Operators
delete_ = string "delete"

void_ = string "void"

typeof_ = string "typeof"

-- ++a
plusplusUnary_ = string "++"

-- --a
minusminusUnary_ = string "--"

plus_ = string "+"

minus_ = string "-"

bitflip_ = string "~"

not_ = string "!"

-- Multiplicative Operators
-- a * b
times_ = string "*"

-- a / b
divides_ = string "/"

-- a % b
modulo_ = string "%"

-- Additive operators
-- a + b
plus_ = string "+"

-- a - b
minus_ = string "-"

-- Bitwise Shift Operators
-- << =
-- >> =
-- >>> =
-- Relational operators
lt_ = string "<"

gt_ = string ">"

leq_ = string "<="

geq_ = string ">="

instanceof_ = string "instaceof"

in_ = string "in"

-- Equality Operators
eqeq_ = string "=="

noteqeq_ = string "!="

eqeqeq_ = string "==="

noteqeqeq_ = string "!=="

-- Binary Bitwise Operators
andBitwise_ = string "&"

xorBitwise_ = string "^"

orBitwise_ = string "|"

--Binary Logical Operators
and_ = string "&&"

or_ = string "||"

-- Conditional Operator
ternary_ = string "?:" -- TODO: Change

-- Assignment Operators
eq_ = string "="

timesEq_ = string "*="

dividesEq_ = string "/="

moduloEq_ = string "%="

plusEq_ = string "+="

minusEq_ = string "-="

leftShiftEq_ = string "<<="

rightShiftEq_ = string ">>="

rightShiftUnsignedEq_ = string ">>>=" -- TODO: Lookup this operator

andBitwiseEq_ = string "&="

xorBitwiseEq_ = string "^="

orBitwiseEq_ = string "|="
