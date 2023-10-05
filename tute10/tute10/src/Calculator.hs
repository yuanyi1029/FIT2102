module Calculator (repl)
where

import Control.Applicative ((<|>), many)
import Instances
import ParserExercises
import Helper

data Expr = Plus Expr Expr | Minus Expr Expr | Times Expr Expr | Number Int
  deriving (Show)

-- Parse a binary operator given as a character, ignoring spaces before and after the character
-- >>> parse (op '+') " +123"
-- Result >123< '+'
-- >>> parse (op '+') " -123"
-- Unexpected character: "-"
op :: Char -> Parser Char
op = undefined

-- | Parse an integer.
--
-- >>> parse number " 123"
-- Result >< Number 123
--
-- >>> parse number " -456"
-- Result >< Number (-456)
--
-- >>> parse number " 0"
-- Result >< Number 0
--
-- >>> parse number " a0"
-- Unexpected character: "a"
number :: Parser Expr -- parse a Number
number = undefined

-- Parse the "+" operator
-- >>> parse (add <*> pure (Number 1) <*> pure (Number 2)) "+"
-- Result >< Plus (Number 1) (Number 2)
-- >>> parse (add <*> pure (Number 1) <*> pure (Number 2)) "*"
-- Unexpected character: "*"
add :: Parser (Expr -> Expr -> Expr)
add = undefined

-- Parse the "-" operator
-- >>> parse (minus <*> pure (Number 1) <*> pure (Number 2)) "-"
-- Result >< Minus (Number 1) (Number 2)
-- >>> parse (minus <*> pure (Number 1) <*> pure (Number 2)) "*"
-- Unexpected character: "*"
minus :: Parser (Expr -> Expr -> Expr)
minus = undefined

-- Parse the "*" operator
-- >>> parse (times <*> pure (Number 1) <*> pure (Number 2)) "*"
-- Result >< Times (Number 1) (Number 2)
-- >>> parse (times <*> pure (Number 1) <*> pure (Number 2)) "+"
-- Unexpected character: "+"
times :: Parser (Expr -> Expr -> Expr)
times = undefined

-- We will be developing this calculator in three stages
-- Stage 1: Parse with explicit brackets
-- Stage 2: Parse with zero brackets
-- Stage 3: (Optional) Parse with optional brackets
-- Once these stages are done, you will be processing a file with a series of equations
-- Then writing a REPL to interact with the user.

-- | *** Stage 1 ***
-- | Parse a number or an explicitly bracketed operation between two numbers.
--
-- /Hint/: This will be easier to do with do notation

-- >>> parse bracketExpr "(1 + 2)"
-- Plus (Number 1) (Number 2)
-- >>> parse bracketExpr "(1 + 2 + 4)"
-- Unexpected character: " "
-- >>> parse bracketExpr "((1 + 2) * 4))"
-- Times (Plus (Number 1) (Number 2)) (Number 4)
bracketExpr :: Parser Expr
bracketExpr =
  number <|> do
    undefined -- What do we do here?

-- | *** Stage 2 ***
-- Make sure you read the course notes: https://tgdwyer.github.io/parsercombinators/
-- 
-- This is the higher precedence operator, so should only parse multiplication
-- /Hint/: Useful function Y2hhaW4=
--
-- >>> parse multiplicationTerms "1"
-- Result >< Number 1
-- >>> parse multiplicationTerms "1*1"
-- Result >< Times (Number 1) (Number 1)
-- >>> parse multiplicationTerms "+1"
-- Unexpected character: "+"
multiplicationTerms :: Parser Expr
multiplicationTerms = undefined

-- | Expression consisting of terms and lower precedence operators
-- You will need to chain multiplication over lower precedence operators
-- >>> parse parseAddMinus " 12 + 21* 3 "
-- Result > < Plus (Number 12) (Times (Number 21) (Number 3))
--
-- >>> parse parseAddMinus " 6 *4 + 333- 8 *  24"
-- Result >< Minus (Plus (Times (Number 6) (Number 4)) (Number 333)) (Times (Number 8) (Number 24))
parseAddMinus :: Parser Expr
parseAddMinus = undefined


-- This should be one of the previous two parsers, which one? and why?
-- >>> parse parseNoBracketCalc " 12 + 21* 3 "
-- Result > < Plus (Number 12) (Times (Number 21) (Number 3))
--
-- >>> parse parseNoBracketCalc " 6 *4 + 333- 8 *  24"
-- Result >< Minus (Plus (Times (Number 6) (Number 4)) (Number 333)) (Times (Number 8) (Number 24))
parseNoBracketCalc :: Parser Expr
parseNoBracketCalc = undefined

-- | *** Stage 3 ***

-- The parens function is meant to parse expressions enclosed in parentheses. Follow these steps to implement it:
-- >>> parse (parens (is 'a')) "(a)"
-- Result >< 'a'
-- >>> parse (parens (is 'a')) "(b)"
-- Unexpected character: "b"
parens :: Parser a -> Parser a
parens p = undefined

-- Parses a number or a low precedence sub-expression enclosed in parentheses
-- >>> parse numberOrParenthesizedExpr "1"
-- Result >< Number 1
-- >>> parse numberOrParenthesizedExpr "(1+34)"
-- Result >< Plus (Number 1) (Number 34)
numberOrParenthesizedExpr :: Parser Expr
numberOrParenthesizedExpr = undefined

-- Chain higherPrecedenceExpr separated by lower precedence operators
-- >>> parse lowerPrecedenceExpr "1+2"
-- Result >< Plus (Number 1) (Number 2)
lowerPrecedenceExpr :: Parser Expr
lowerPrecedenceExpr = undefined

-- Chain the multiplication operator over either a number or parenthesized expression
-- >>> parse higherPrecedenceExpr "1 * 2"
-- Result >< Times (Number 1) (Number 2)
-- >>> parse higherPrecedenceExpr "(1 * 2)"
-- Result >< Times (Number 1) (Number 2)
higherPrecedenceExpr  :: Parser Expr
higherPrecedenceExpr = undefined

-- This should be one of the previous three parsers, which one? and why?
-- >>> parse parseFullCalc "(1+2+3*(4+5))+10*100-5+4"
-- Result >< Plus (Minus (Plus (Plus (Plus (Number 1) (Number 2)) (Times (Number 3) (Plus (Number 4) (Number 5)))) (Times (Number 10) (Number 100))) (Number 5)) (Number 4)
-- >>> parse parseFullCalc "(1+2+3"
-- Unexpected end of stream
-- >>> parse parseFullCalc "(1+2+3)"
-- Result >< Plus (Plus (Number 1) (Number 2)) (Number 3)
parseFullCalc :: Parser Expr
parseFullCalc = undefined

-- Now we have a parser which can handle any mathematical expression, lets evaluate the expression
-- Your parser should return expression with correct precedence.
-- >>> evalExpr (Number 5)
-- 5
-- >>> evalExpr (Times (Number 4) (Plus (Number 1) (Number 2)))
-- 12
-- >>> evalExpr (Plus (Minus (Plus (Plus (Plus (Number 1) (Number 2)) (Times (Number 3) (Plus (Number 4) (Number 5)))) (Times (Number 10) (Number 100))) (Number 5)) (Number 4))
-- 1029
evalExpr :: Expr -> Int
evalExpr _ = undefined

-- Print the result to the screen
-- If the parse is successful, prepend "Result: " to the calculated value
-- Otherwise, just show the error message
-- >>> showExprResult (Result "" (Number 12))
-- Result: 12
-- >>> showExprResult (Result "" (Plus (Number 12) (Number 2)))
-- Result: 14
-- >>> showExprResult (Error UnexpectedEof)
-- Unexpected end of stream
showExprResult :: ParseResult Expr -> IO ()
showExprResult _ = undefined

-- Given a filename, evaluate every line in the file
-- This will require parsing the line, and then evaluating in to a single number
-- /Hint/: Use readFile to read a file to a string
-- /Hint 2/: Other useful functions: bWFwTV8sIGxpbmVz
-- >>> processFile "share/sample_eqns.txt"
-- Result: 28
-- Result: 32
-- Result: 36
processFile :: FilePath -> IO ()
processFile fileName = undefined

-- Instructions:
-- Your task is to complete the implementation of the `repl` function to create a
-- Read-Eval-Print Loop (REPL) for evaluating arithmetic expressions. Follow the
-- instructions below to use the REPL:
--
-- 1. Start the REPL:
--    - Run the program, and it will display the initial prompt:
--      ```
--      Enter an arithmetic expression, 'f' to enter a file name, or 'q' to quit:
--      ```
--
-- 2. Enter an Expression:
--    - To evaluate an arithmetic expression, simply type the expression and press Enter.
--      For example, you can enter `1+2*3` and press Enter.
--
-- 3. View the Result:
--    - After entering an expression, the program will parse and evaluate it, and then
--      display the result. For example:
--      ```
--      Enter an arithmetic expression, 'f' to enter a file name, or 'q' to quit:
--      1+2*3
--      Result: 7
--      ```
--
-- 4. Load Expressions from a File:
--    - To load expressions from a file, type `f` and press Enter. The program will prompt
--      you to enter the file name.
--    - Enter the name of the file that contains arithmetic expressions. For example, if
--      you have a file named "expressions.txt," you can enter `expressions.txt` and press
--      Enter.
--    - The program will parse and evaluate expressions from the file and display the results.
--
-- 5. Quit the Program:
--    - To quit the program, simply type `q` and press Enter. The program will display
--      "Goodbye!" and exit.
--
-- 6. Repeat:
--    - After each operation (entering an expression, loading from a file, or quitting),
--      the program will return to the repl prompt, allowing you to perform another action.
--
-- Your task is to complete and test the `repl` function to ensure that the REPL behaves
-- according to the instructions above. You will need to replace all undefined with correct
-- behavior. Good luck!
-- /Hint/: This will need to be recursive
repl :: IO ()
repl = do
  putStrLn "Enter an arithmetic expression, 'f' to enter a file name or 'q' to quit:"
  input <- undefined 
  case input of
    "q" -> do
      undefined
    "f" -> do
      undefined
    eqn -> do
      undefined
