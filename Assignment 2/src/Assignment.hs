--This module contains the skeleton code for the assignment.
--
-- Please do not change the names of the parseExerciseX functions, as they
-- are used by the test suite.
--
-- You what may, and are highly encouraged, to create your functions.
module Assignment where

import Control.Applicative
import Instances
import Parser
import Data.List (intercalate)

-- Data Type Declaration for Binary Operators 
data Operator 
  = And 
  | Or 
  | Plus  
  | Minus  
  | Times 
  | Divide 
  | Power 
  | Equal 
  | Unequal  
  | Greater 
  | Smaller

-- Show Instance for Binary Operators 
instance Show Operator where
  show And = "&&"
  show Or = "||"
  show Plus = "+"
  show Minus = "-"
  show Times = "*"
  show Divide = "/"
  show Power = "**"
  show Equal= "==="
  show Unequal = "!=="
  show Greater = ">"
  show Smaller = "&&"

-- Data Type Declaration for ADT
data ADT 
  = Empty
  -- Data Types for Part A
  | AInteger Int
  | AString String
  | ATrue 
  | AFalse
  | AList [ADT]
  | ANot ADT 
  | AExpr Operator ADT ADT
  | ATernary ADT ADT ADT

  -- Data Types for Part B
  | AVarName String
  | AConst ADT ADT
  | AConsts [ADT]
  | ABlock [ADT]
  | AIf ADT ADT
  | AIfElse ADT ADT
  | AStatements [ADT]

  -- Data Types for Part C
  | AFunctionCall ADT [ADT] 
  | AFunctionCallEnd ADT [ADT] 
  | AFunction ADT ADT 
  | AReturn ADT
  | AWhile ADT 
  | ADestructure [ADT] [ADT]

-- Show Instance for ADT
instance Show ADT where
  show (AInteger value) = show value
  show (AString value) = show value
  show ATrue = "true"
  show AFalse = "false"
  show (AList value) = show value
  show (ANot value) = "(!" ++ show value ++ ")"
  show (AExpr op lhs rhs) = 
    "(" ++ show lhs ++ " " ++ show op ++ " " ++ show rhs ++ ")"
  show (ATernary condition pass crash) 
    | isMultiline (ATernary condition pass crash) = 
      "(" ++ 
      show condition ++ "\n? " ++ show pass ++ "\n: " ++ show crash ++ 
      ")"
    | otherwise = 
      "(" ++
      show condition ++ " ? " ++ show pass ++ " : " ++ show crash ++ 
      ")"

  show (AVarName vName) = vName
  show (AConst vName expr) = 
    "const " ++ show vName ++ " = " ++ show expr  ++ ";"
  show (AConsts consts) = intercalate "\n" (map show consts)
  show (ABlock block) 
    | null block = "{ }"
    | isMultiline (ABlock block) = multiBlock (ABlock block) "" False
    | otherwise = "{ " ++ show (head block) ++ " }"
  show (AIf expr ifBlock) 
    | isMultiline (AIf expr ifBlock) = multiIf (AIf expr ifBlock) ""
    | otherwise = "if (" ++ show expr ++ ") " ++ show ifBlock
  show (AIfElse aIf elseBlock) 
    | isMultiline (AIfElse aIf elseBlock) = 
        multiIfElse (AIfElse aIf elseBlock) ""
    | otherwise = show aIf ++ " else " ++ show elseBlock
  show (AStatements lst) = intercalate "\n" (map show lst)
  
  show (AFunctionCall fcName args) = 
    show fcName ++ "(" ++ intercalate ", " (map show args) ++ ")"
  show (AFunctionCallEnd fcName args) = 
    show fcName ++ "(" ++ intercalate ", " (map show args) ++ ");"
  show (AFunction fCall block)
    | isTailRecursive ("function " ++ show fCall ++ " " ++ show block) = 
        show (replaceReturnAddWhile (AFunction fCall block))
    | otherwise = "function " ++ show fCall ++ " " ++ show block
  show (AReturn expr) = "return " ++ show expr ++ ";"
  show (AWhile block) = "while (true) " ++ show block
  show (ADestructure args1 args2) = 
    "[" ++ 
    intercalate ", " (map show args1) ++ "] = [" ++ 
    intercalate ", " (map show args2) ++ 
    "];"

  show Empty = ""

-- Check if a given ADT is multiline, only Ternary, multiple consts, code  
-- blocks, if and if else statements, functions and while loops can be
-- multiline
isMultiline :: ADT ->  Bool
isMultiline (ATernary condition pass crash) 
  =  isMultiline condition 
  || isMultiline pass 
  || isMultiline crash 
  || length (
      "(" ++
      show condition ++ " ? " ++ show pass ++ " : " ++ show crash ++
      ")"
    ) > 42
isMultiline (AConsts _) = True
isMultiline (ABlock block) 
  =  any isMultiline block 
  || length block > 1 
  || length ("{ " ++ concatMap show block ++  " }") > 42
isMultiline (AIf condition block) 
  =  isMultiline condition 
  || isMultiline block 
  || length ("if (" ++ show condition ++ ") " ++ show block) > 42
isMultiline (AIfElse ifStatement block) 
  =  isMultiline ifStatement 
  || isMultiline block 
  ||  length (show ifStatement ++ " else " ++ show block) > 42
isMultiline (AStatements statements) 
  =  any isMultiline statements 
  || length statements > 1 
  || length (concatMap show statements) > 42
isMultiline (AFunction fCall block) 
  =  isMultiline fCall 
  || isMultiline fCall 
  || length ("function " ++ show fCall ++ " " ++ show block) > 42
isMultiline (AWhile block) 
  =  isMultiline block 
  || length ("while (true) " ++ show block) > 42
isMultiline _ = False

-- Multiline pretty print function for "ABlock" type, called when a block is 
-- multiline, used in show instance of ADT 
multiBlock :: ADT -> String -> Bool -> String
multiBlock (ABlock block) prefix single = 
  (if single then "" else prefix) ++ "{\n" ++ 
  intercalate "\n" 
    (map (\each -> multiBlock each (prefix ++ "  ") False) block) ++ 
  "\n" ++ prefix ++ "}" 
multiBlock (AIf expr ifBlock) prefix _ = 
  multiIf (AIf expr ifBlock) prefix
multiBlock (AIfElse aIf elseBlock) prefix _ = 
  multiIfElse (AIfElse aIf elseBlock) prefix
multiBlock (AWhile block) prefix _ = 
  multiWhile (AWhile block) prefix
multiBlock v prefix _ = prefix ++ show v

-- Multiline pretty print function for "AIf" type, called when a if statement 
-- is multiline, used in show instance of ADT 
multiIf :: ADT -> String -> String 
multiIf (AIf expr ifBlock) prefix = 
  prefix ++ "if (" ++ show expr ++ ") " ++ multiBlock ifBlock prefix True
multiIf _ _ = ""

-- Multiline pretty print function for "AIfElse" type, called when a if else 
-- statement is multiline, used in show instance of ADT 
multiIfElse :: ADT -> String -> String
multiIfElse (AIfElse aIf ifBlock) prefix = 
  multiIf aIf prefix ++ " else " ++ multiBlock ifBlock prefix True
multiIfElse _ _ = ""

-- Multiline pretty print function for "AWhile" type, called when a while 
-- loop statement is multiline, used in show instance of ADT 
multiWhile :: ADT -> String -> String
multiWhile (AWhile block) prefix = 
  prefix ++ "while (true) " ++ multiBlock block prefix True
multiWhile _ _ = ""

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- Combines multiple parsers, allowing to parse various expressions, 
-- used in unary, binary and ternary expressions throughout Part A,
-- Part B and even Part C
jsExprP :: Parser ADT
jsExprP 
  = integerP
  <|> boolP 
  <|> stringP  
  <|> listP 
  <|> logicExprP
  <|> arithExprP 
  <|> equivalenceExprP
  <|> ternaryExprP
  <|> functionCallP
  <|> returnP
  <|> jsVarName

-- Combines multiple parsers, allowing to parse various statements, 
-- used in parsing blocks such as code blocks, if and if else blocks,
-- and function blocks throughout Part B and Part C
jsStatementP :: Parser ADT
jsStatementP = manyConstP <|> blockP <|> ifElseP <|> returnP <|> whileP 

-----------------------------------------------------------------------------
---------------------------------- Part A -----------------------------------
-----------------------------------------------------------------------------
-- Exercise 1: Parsing Integers, Strings and Boolean literals

-- Parses an integer into type "AInteger" of ADT
integerP :: Parser ADT  
integerP = do 
  n <- int
  _ <- spaces
  pure (AInteger n)

-- Parses a string with quotation marks into type "AString" of ADT
stringP :: Parser ADT
stringP =  charTok2 '"' *> (AString <$> many (isNot '"')) <* charTok2 '"'

-- Parses a "true" into type "ATrue" of ADT
trueP :: Parser ADT
trueP = ATrue <$ stringTok2 "true"

-- Parses a "false" into type "AFalse" of ADT
falseP :: Parser ADT
falseP = AFalse <$ stringTok2 "false"

-- Combines trueP and falseP, allowing to parse "true" or "false"
-- boolP is allowed to wrap around parenthesis due to last test case in Part B
boolP :: Parser ADT
boolP = trueP <|> falseP <|> parens (trueP <|> falseP)

-- Parses a multiple expressions seperated by commas, wrapped around square 
-- brackets into type "AList" of ADT
listP :: Parser ADT
listP = charTok2 '[' *> (AList <$> sepBy jsExprP commaTok2) <* charTok2 ']'

-- Exercise 2: Parsing Unary / Binary Operator Expressions

-- Parses a "&&" into type "And" of Operator
andP :: Parser Operator
andP = stringTok2 "&&" >> pure And

-- Parses a "||" into type "Or" of Operator
orP :: Parser Operator
orP = stringTok2 "||" >> pure Or

-- Parses a '!' into type "ANot" of ADT
notP :: Parser  (ADT -> ADT)
notP = charTok2 '!' >> pure ANot

-- Parses a expression, "&&" or "||" and another expression, wrapped around
-- parenthesis into type "AExpr" of ADT, or a '!' and an expression, wrapped
-- around parenthesis to construct a "ANot" of ADT
logicExprP :: Parser ADT
logicExprP = 
  parens (do
    v1 <- jsExprP
    o <- andP <|> orP
    AExpr o v1 <$> jsExprP) <|>
  parens (notP <*> jsExprP)

-- Parses a '+' into type "Plus" of Operator
plusP :: Parser Operator
plusP = charTok2 '+' >> pure Plus

-- Parses a '-' into type "Minus" of Operator
minusP :: Parser Operator
minusP = charTok2 '-' >> pure Minus

-- Parses a '*' into type "Times" of Operator
timesP :: Parser Operator
timesP = charTok2 '*' >> pure Times

-- Parses a '/' into type "Divide" of Operator
divideP :: Parser Operator
divideP = charTok2 '/' >> pure Divide

-- Parses a "**" into type "Power" of Operator
powerP :: Parser Operator
powerP = stringTok2 "**" >> pure Power

-- Combines all airthmetic operator parsers, allowing to parse any operator
operatorsP :: Parser Operator
operatorsP = powerP <|> plusP <|> minusP <|> timesP <|> divideP

-- Parses a expression, an arithmetic operator and another expression, wrapped
-- around parenthesis into type "AExpr" of ADT 
arithExprP :: Parser ADT
arithExprP = 
  parens (do
    v1 <- jsExprP
    o <- operatorsP
    AExpr o v1 <$> jsExprP)

-- Parses a "===" into type "Equal" of Operator
equalP :: Parser Operator
equalP = stringTok2 "===" >> pure Equal

-- Parses a "!==" into type "Unequal" of Operator
unequalP :: Parser Operator
unequalP = stringTok2 "!==" >> pure Unequal

-- Parses a '>' into type "Greater" of Operator
greaterP :: Parser Operator
greaterP = charTok2 '>' >> pure Greater

-- Parses a '<' into type "Smaller" of Operator
smallerP :: Parser Operator
smallerP = charTok2 '<' >> pure Smaller

-- Combines all comparison parsers, allowing to parse any operator
comparisonsP :: Parser Operator
comparisonsP = equalP <|> unequalP <|> greaterP <|> smallerP

-- Parses a expression, an comparison operator and another expression, 
-- wrapped around parenthesis into type "AExpr" of ADT 
equivalenceExprP :: Parser ADT
equivalenceExprP = 
  parens (do
    v1 <- jsExprP
    o <- comparisonsP
    AExpr o v1 <$> jsExprP) 

-- Exercise 3: Parsing Ternary Expressions

-- Parses a expression, a '?' and another expression, followed by a ':' and a
-- final expression into type "ATernary" of ADT
ternaryExprP :: Parser ADT
ternaryExprP = 
  parens (do
    v1 <- jsExprP
    _ <- charTok2 '?'
    v2 <- jsExprP
    _ <- charTok2 ':'
    ATernary v1 v2 <$> jsExprP)

-----------------------------------------------------------------------------
---------------------------------- Part B -----------------------------------
-----------------------------------------------------------------------------
-- Exercise 1: Parsing const declarations

-- Parses ASCII characters into type "AVarName" of ADT
jsVarName :: Parser ADT
jsVarName = 
  AVarName <$> (spaces *> some (alpha <|> digit <|> is '_') <* spaces)

-- Parses a "const" string, a variable name, followed by a "=" and a
-- expression into type "AConst" of ADT
singleConstP :: Parser ADT
singleConstP = do
  _ <- stringTok2 "const"
  v1 <- jsVarName
  _ <- charTok2 '='
  v2 <- jsExprP 
  _ <- charTok2 ';'
  pure (AConst v1 v2)

-- Parses a one or more const declarations seperated by spaces into type 
-- "AConsts" of ADT
manyConstP :: Parser ADT
manyConstP = do 
  lst <- some (singleConstP <* spaces)
  pure (AConsts lst)

-- Exercise 2: Parsing blocks

-- Parses a one or more statements seperated by spaces into type 
-- "ABlock" of ADT, flattening "AConsts" into "AConst" if possible
blockP :: Parser ADT
blockP = do
  _ <- charTok2 '{'
  lst <- sepBy jsStatementP spaces
  _ <- charTok2 '}'
  pure (ABlock (concatMap flattenConsts lst))

-- Helper function to flatten multiple consts "AConsts" into single consts
-- "AConst" for easier handling
flattenConsts :: ADT -> [ADT]
flattenConsts (AConsts consts) = consts
flattenConsts x = [x]

-- Exercise 3: Parsing conditional structures

-- Parses a "if" string, an expression wrapped in parenthesis, followed by a 
-- a block into type "AIf" of ADT
ifP :: Parser ADT
ifP = do
  _ <- stringTok2 "if"
  i1 <- parens jsExprP
  AIf i1 <$> blockP

-- Parses an entire "AIf" object, a "else" string, followed by a block into 
-- type "AIfElse" of ADT
elseP :: Parser ADT
elseP = do
  i1 <- ifP
  _ <- stringTok2 "else"
  AIfElse i1 <$> blockP

-- Combines the if and if else parsers, allowing to parse "AIf" and "AIfElse"
ifElseP :: Parser ADT
ifElseP = elseP <|> ifP

-----------------------------------------------------------------------------
---------------------------------- Part C -----------------------------------
-----------------------------------------------------------------------------
-- Exercise 1: Parsing function calls

-- Parses a "do" string, a variable name, multiple expressions separated by 
-- commas wrapped in parenthesis, into type "AFunctionCall" of ADT
functionCallP :: Parser ADT 
functionCallP = do
  v1 <- jsVarName
  _ <- charTok2 '('
  v2 <- sepBy jsExprP commaTok2
  _ <- charTok2 ')'
  pure (AFunctionCall v1 v2)

-- The same parser as functionCallP, but parses an extra ';' at the end, into
-- type "AFunctionCallEnd" of ADT
functionCallEndP :: Parser ADT
functionCallEndP = do
  v1 <- jsVarName
  _ <- charTok2 '('
  v2 <- sepBy jsExprP commaTok2
  _ <- charTok2 ')'
  _ <- charTok2 ';'
  pure (AFunctionCallEnd v1 v2)

-- Exercise 2: Parsing function structures with return statement

-- Parses a "function" string, a function call, followed by a block, into 
-- type "AFunction" of ADT
functionP :: Parser ADT
functionP = do
  _ <- stringTok2 "function"
  v1 <- functionCallP
  AFunction v1 <$> blockP

-- Parses a "return" string, a expression followed by a ';' into type  
-- "AReturn" of ADT
returnP :: Parser ADT
returnP = do 
  _ <- stringTok2 "return"
  v1 <- jsExprP
  _ <- charTok2 ';'
  pure (AReturn v1)

-- Exercise 3: Check that a function is tail recursive

-- Helper function to count the total number of returns in a block 
-- (Criteria 2)
countReturn :: ADT -> Int
countReturn (AReturn expr) = if checkArgsValid expr then 1 else 0
countReturn (ABlock block) = sum (map countReturn block)  
countReturn (AIf _ ifBlock) = countReturn ifBlock 
countReturn (AIfElse (AIf _ ifBlock) elseBlock) = 
  countReturn ifBlock + countReturn elseBlock 
countReturn _ = 0  

-- Helper function to determine if a given expression contains a return
-- statement (Criteria 2)
checkReturn :: ADT -> Bool
checkReturn (AReturn _) = True
checkReturn (ABlock block) = any checkReturn block
checkReturn (AIf _ ifBlock) = checkReturn ifBlock
checkReturn (AIfElse (AIf _ ifBlock) elseBlock) = 
  checkReturn ifBlock || checkReturn elseBlock
checkReturn _ = False

-- Check if the last return value of a block is a valid function call with the 
-- same name and number of arguments as the initial function call (Criteria 3)
lastReturnValid :: ADT -> (String -> Bool) -> Int -> Bool
lastReturnValid (AReturn (AFunctionCall (AVarName fcName) args)) f2 argLen = 
  f2 fcName && all checkArgsValid args && length args == argLen
lastReturnValid _ _ _ = False

-- Helper function to compare function call name is the same as the return 
-- function name (Criteria 3)
compareFName :: String -> String -> Bool
compareFName fName fcName = fcName == fName

-- Helper function to check if a return statement's arguments are valid
-- where return statements cannot have function calls (Criteria 2 and 
-- Criteria 3)
checkArgsValid :: ADT -> Bool
checkArgsValid (AFunctionCall _ _) = False
checkArgsValid (AList lst) = all checkArgsValid lst
checkArgsValid (ANot expr) = checkArgsValid expr
checkArgsValid (AExpr _ expr1 expr2) = 
  checkArgsValid expr1 || checkArgsValid expr2
checkArgsValid (ATernary condition pass crash) = 
  checkArgsValid condition || checkArgsValid pass || checkArgsValid crash
checkArgsValid _ = True 

-- Exercise 4: Parsing function structures with return statement

-- Helper function to wrap an existing block with a while loop of type 
-- "AWhile" of ADT, used when a data structure is tail recursive, add
-- a while loop before printing
addWhile :: ADT -> ADT 
addWhile (AFunction fCall (ABlock block)) = 
  AFunction fCall (ABlock [AWhile (ABlock block)])
addWhile _ = Empty 

-- Helper function to replace the final return statement of a block with a
-- destructure data type of type "ADestructure" of ADT, used when a data 
-- structure is tail recursive, replace final return statement with a 
-- destructure data type before printing
replaceReturn :: ADT -> ADT
replaceReturn (AFunction (AFunctionCall fcName fcArgs) (ABlock block)) = 
  case last block of 
    (AReturn (AFunctionCall _ rArgs)) -> 
      AFunction 
      (AFunctionCall fcName fcArgs) 
      (ABlock (init block ++ [ADestructure fcArgs rArgs]))
    _ -> AFunction (AFunctionCall fcName fcArgs) (ABlock block)
replaceReturn _ = Empty

-- Helper function to add replace the final return statement and the add
-- a while loop
replaceReturnAddWhile :: ADT -> ADT
replaceReturnAddWhile = addWhile . replaceReturn

-- Parses a "while (true)" string, followed by a block, into type "AWhile"
-- of ADT, assume it is only used in tail recursion optimisation
whileP :: Parser ADT
whileP = do
  _ <- stringTok2 "while (true)"
  AWhile <$> blockP

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-- | Exercise A

-- Parses integers, strings, booleans, lists, binary and ternary expressions 
parseExerciseA :: Parser ADT
parseExerciseA 
  = integerP
  <|> boolP 
  <|> stringP  
  <|> listP 
  <|> logicExprP
  <|> arithExprP 
  <|> equivalenceExprP
  <|> ternaryExprP

prettyPrintExerciseA :: ADT -> String 
prettyPrintExerciseA = show 

-- | Exercise B

-- Parses many const declarations, blocks, if and if else statements 
-- separated by spaces into type "AStatements" of ADT
parseExerciseB :: Parser ADT
parseExerciseB = do
  lst <- some (manyConstP <|> blockP <|> ifElseP <* spaces)
  pure (AStatements lst)

prettyPrintExerciseB :: ADT -> String 
prettyPrintExerciseB = show

-- | Exercise C

-- This function should determine if given code is a tail recursive function
-- Checks for criteria 1 at pattern matching
-- Checks for criteria 2 at:
--    countReturn (ABlock block) >= 1
-- Checks for criteria 3 at:
--    lastReturnValid (last block) (compareFName fName) (length args)
isTailRecursive :: String -> Bool
isTailRecursive input = case parse functionP input of 
  Result _ (AFunction (AFunctionCall (AVarName fName) args) (ABlock block)) -> 
    countReturn (ABlock block) >= 1 && 
    lastReturnValid (last block) (compareFName fName) (length args)
  _ -> False

-- Parses function calls and functions, required to be able to parse 
-- statements from Part B due to the test cases having const declarations
parseExerciseC :: Parser ADT
parseExerciseC 
  = parseExerciseB
  <|> functionCallEndP
  <|> functionP

prettyPrintExerciseC :: ADT -> String
prettyPrintExerciseC = show 
