{-# LANGUAGE InstanceSigs #-}

-- |
--  Previously we have seen parsing functions that look like this:
--   String -> Maybe (String, a)
--  but we don't have a good way of composing these functions together, or
--  operating on the parsed values without manually extracting them.
--
--  We will now use the typeclasses Functor and Applicative to make this easier.
--
--  see https://tgdwyer.github.io/haskell3/#a-simple-applicative-functor-for-parsing
module Parser
  ( Parser (..),
    parse,
    is,
    char,
    int,
    open,
    close,
    comma,
  )
where

-- You may add more imports as you wish/need
import Control.Applicative
  ( Alternative (empty, many, (<|>)),
    Applicative (liftA2),
  )

-- $setup
-- import Control.Applicative
-- import Data.Char

-- | This is our Parser which holds a parsing function.
--
--   The function returns
--      - Nothing, if the parsing fails
--      - Just (r, p), where r is the unparsed portion of the input,
--        and p is the parsed input
newtype Parser a = Parser (String -> Maybe (String, a))

-- | Wrapper function for parsing.
--
-- This just extracts the function from a Parser value.
--
-- >>> f s = Just ("", s)
-- >>> parse (Parser f) "abc"
-- Just ("","abc")
parse :: Parser a -> (String -> Maybe (String, a))
parse (Parser p) = p

-- | Parse a single character
--
-- >>> parse char "abc"
-- Just ("bc",'a')
--
-- >>> parse char ""
-- Nothing
char :: Parser Char
char = Parser f
  where
    f "" = Nothing
    f (x : xs) = Just (xs, x)

-- | Parse numbers as int until non-digit
--
-- >>> parse int "123abc"
-- Just ("abc",123)
--
-- >>> parse int "abc"
-- Nothing
int :: Parser Int
int = Parser f
  where
    f s = case reads s of
      [(x, rest)] -> Just (rest, x)
      _ -> Nothing

-- | Applies the mapping function to the *result* (parsed value) of the parser.
--
-- see https://tgdwyer.github.io/haskell3/#functor
--
-- /Hint/: We need to "thread" the mapping function through the return
--  value of the parsing function Maybe (String, a)
--
-- /Hint 2/: You will have to manually construct a parsing function and parser.
--
-- /Hint 3/: Think about each step carefully
--
--   The returned parser should do the following:
--
--   - Parser with a function that takes in some input string
--
--   - Executes the input parser on the input string, then
--
--     - If the parse is successful (Just value)
--
--       - Take the parsed value and remainder
--
--         - Return a Just value with the remainder, and
--         - the mapping function applied to the parsed value
--
--     - If the parse is unsuccessful, return Nothing
--
--
-- /Challenge/: Write this using fmaps
--
--   There are 3 layers of functors here:
--    - the function ((->) String)
--    - The Maybe
--    - The tuple (String, a)
--
--   /Hint/: Can we make use of the functor instances for these types?
--    What does fmap do for each of these types?
--
--   /Hint 2/: Using fmap dGhyZWUgbGF5ZXJzIG9mIGZ1bmN0b3JzLCB0aHJlZSBmbWFwcz8=
--
-- /End Challenge/:
--
-- >>> import Data.Char (toUpper)
-- >>> parse (toUpper <$> char) "abc"
-- Just ("bc",'A')
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser $ \s -> case p s of 
    Just (rest, result) -> Just (f <$> (rest, result))
    _ -> Nothing

-- |
--
-- Similar to Functor, we want to apply the function to the parsed value.
--
-- The "effect" of the Parser type is in two parts:
--
-- 1. If the Parser fails at any point, that is the parsing function returns
--    a Nothing value, then all future parsing functions should also return
--    a Nothing value.
--
-- 2. The remaining input after parsing should be passed as input to the
--    next parsing function.
--
-- see https://tgdwyer.github.io/haskell3/#applicative
--
-- >>> parse (pure 1) "abc"
-- Just ("abc",1)
--
-- >>> parse (pure (+1) <*> int) "123abc"
-- Just ("abc",124)
--
-- >>> parse (is '(' *> is 'a') "(a"
-- Just ("",'a')
--
-- >>> parse (liftA2 (+) int (is '+' *> int)) "1+2"
-- Just ("",3)
instance Applicative Parser where
  -- Returns a parser that always succeeds with the given value,
  -- ignoring the input.
  pure :: a -> Parser a
  pure a = Parser $ \s -> Just (s, a)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p1@(Parser a) <*> p2@(Parser b) = Parser $ \s -> case a s of
    Just (rest, f) -> case b rest of
      Just (rest', result) -> Just (rest', f result)
      _ -> Nothing
    _ -> Nothing

-- |
--
-- 'Alternative' is used for parsers that support choice (alternative) and
-- failure (empty). In the context of this 'Parser' type:
--
-- 1. 'empty' represents a parser that always fails
--
-- 2. '<|>' combines two parsers. It tries the first parser on the input,
-- and if it fails it tries the second parser.
--
-- see https://tgdwyer.github.io/haskell3/#alternative
--
-- >>> parse (int <|> pure 0) "123abc"
-- Just ("abc",123)
--
-- >>> parse (is 'h' <|> is 'w') "world, hello!"
-- Just ("orld, hello!",'w')
--
-- >>> parse (int <|> empty) "world"
-- Nothing
instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser p1) (Parser p2) = Parser $ \s -> case p1 s of 
    Just (rest, result) -> Just (rest, result)
    Nothing -> case p2 s of 
      Just (rest', result') -> Just (rest', result')
      Nothing -> Nothing

-- | Parses a specific character, otherwise return Nothing
--
-- There are better ways to do this, but we won't see it until the
-- later weeks.
--
-- >>> parse (is 'c') "cba"
-- Just ("ba",'c')
-- >>> parse (is 'c') "abc"
-- Nothing
is :: Char -> Parser Char
is c = Parser f
  where
    f i = case parse char i of
      Just (r1, x) | x == c -> Just (r1, x)
      _ -> Nothing

-- | For the following exercises, you must use the functions and parsers
-- defined above.
--
-- You must NOT use case of, guards, pattern matching, explicit construction
-- of a Parser value, or equivalent.

-- | Open round bracket
open :: Parser Char
open = is '('

-- | Close round bracket
close :: Parser Char
close = is ')'

-- | Comma
comma :: Parser Char
comma = is ','

-- | Parse a tuple with two integers
--
-- /Hint/: What are the functions available to us that operate on
-- Applicative instances?
--
-- /Hint 2/: Useful functions U2VhcmNoIHRoZXNlIHVwIG9uIEhvb2dsZSEKbGlmdEEyCigqPikKKDwqKQ==
--
-- >>> parse intTuple2 "(10,2)"
-- Just ("",(10,2))
--
-- >>> parse intTuple2 "[10,2)"
-- Nothing
intTuple2 :: Parser (Int, Int)
intTuple2 = liftA2 (,) (open *> int <* comma) (int <* close)

-- | Parse a list of integers
--
-- /Hint/: What functions does Alternative give us?
--
-- /Hint 2/: Useful functions bGlmdEEyLCBtYW55
--
-- >>> parse list "1,2,3"
-- Just ("",[1,2,3])
--
-- >>> parse list "hello world"
-- Nothing
list :: Parser [Int] 
list = liftA2 (:) int (many (comma *> int))
