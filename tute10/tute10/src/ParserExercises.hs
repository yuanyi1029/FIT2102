module ParserExercises where

import Control.Applicative (Alternative (empty, (<|>)))
import qualified Numeric as N
import Instances 
import Data.Char
  ( isSpace,
    isUpper,
  )
-- | `chain p op` parses 1 or more instances of `p` separated by `op`
-- | (see chainl1 from Text.Parsec)
-- | This is a a very very useful parser combinator for this week
chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p op = p >>= rest
  where
    rest a =
      ( do
          f <- op
          b <- p
          rest (f a b)
      )
        <|> pure a

-- | Return a parser that succeeds with a character off the input or fails with
-- an error if the input is empty.
--
-- >>> parse char "abc"
-- Result >bc< 'a'
--
-- >>> parse char ""
-- Unexpected end of stream
char :: Parser Char
char = P f
  where
    f "" = Error UnexpectedEof
    f (x : xs) = Result xs x

-- | Produces a parser that always fails with 'UnexpectedChar' using the given
-- character.
unexpectedCharParser :: Char -> Parser a
unexpectedCharParser = P . const . Error . UnexpectedChar

-- | -------------------------------------------------
-- | --------------- Satisfy parsers -----------------
-- | -------------------------------------------------
-- | All of these parsers use the `satisfy` parser!
-- | /Hint/: Write a helper function!
-- Use bind for this!
-- | Return a parser that produces a character but fails if:
--
--   * the input is empty; or
--
--   * the character does not satisfy the given predicate.
--
-- >>> parse (satisfy isUpper) "Abc"
-- Result >bc< 'A'
--
-- >>> parse (satisfy isUpper) "abc"
-- Unexpected character: "a"
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = undefined

-- | Return a parser that produces the given character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not equal to the given character.
--
-- You must use satisfy for this exercise
-- >>> parse (is 'c') "c"
-- Result >< 'c'
--
-- >>> parse (is 'c') ""
-- Unexpected end of stream
--
-- >>> parse (is 'c') "b"
-- Unexpected character: "b"
is :: Char -> Parser Char
is = undefined

-- | Return a parser that produces any character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is equal to the given character.
--
-- You must use satisfy for this exercise
-- >>> parse (isNot 'c') "b"
-- Result >< 'b'
--
-- >>> parse (isNot 'c') ""
-- Unexpected end of stream
--
-- >>> parse (isNot 'c') "c"
-- Unexpected character: "c"
isNot :: Char -> Parser Char
isNot = undefined

-- | Return a parser that produces a space character but fails if
--
--   * the input is empty; or
--
--   * the produced character is not a space.
--
-- /Hint/: Use the 'isSpace' function
-- You must use satisfy for this exercise
-- >>> parse space " abc"
-- Result >abc< ' '
-- >>> parse space ""
-- Unexpected end of stream
space :: Parser Char
space = undefined

