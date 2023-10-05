-- | More Parsers
--
-- We will have a look at more interesting things we can do now that we have
-- the Parser type and Functor and Applicative instances. These exercises will
-- require a combination of many concepts we have seen so far.
--
-- If you get stuck on any of these exercises, feel free to consult the
-- base-64 encoded hints that can be decoded at https://www.base64decode.org/
--
-- Also a reminder that type holes exist and can be quite useful!
--
-- The README contains a list of useful functions and operators worth looking at.
--
-- see https://tgdwyer.github.io/haskell3/#a-simple-applicative-functor-for-parsing
-- (optional) see https://tgdwyer.github.io/parsercombinators/
module Parser
  ( char,
    is,
    isNot,
    string,
    nothing,
    just,
    parseMaybe,
    spaces,
    spaces1,
  )
where

import Control.Applicative (Alternative (many, some, (<|>)), Applicative (liftA2))
import Control.Monad (replicateM)
import Data.Char (isSpace)
import Data.Functor (($>))
import Instances (Parser (..), int)

-- | Parse a single character
char :: Parser Char
char = Parser f
  where
    f "" = Nothing
    f (x : xs) = Just (xs, x) 

-- | Parses a specific character, otherwise return Nothing
--
-- There are better ways to do this, but we won't see it until the
-- later weeks.
is :: Char -> Parser Char
is c = Parser f
  where
    f i = case parse char i of
      Just (r1, x) | x == c -> Just (r1, x)
      _ -> Nothing

-- | Parses anything except a specific character, otherwise return Nothing
--
-- There are better ways to do this, but we won't see it until the
-- later weeks.
isNot :: Char -> Parser Char
isNot c = Parser f
  where 
    f i = case parse char i of 
      Just (r1, x) | x /= c -> Just (r1, x)
      _ -> Nothing

-- | Parse a whitespace character
-- This includes more than just the space, but also
-- tabs (\t), \n, \r, \f, \v.
--
-- /Hint/: `isSpace` (check hoogle for what it does)
--
-- There are better ways to do this, but we won't see it until the
-- later weeks.
space :: Parser Char
space = Parser f 
  where 
    f i = case parse char i of 
      Just (r1, x) | isSpace x -> Just (r1, x)
      _ -> Nothing

-- | Write a function that parses the given string (fails otherwise).
--
-- see: https://tgdwyer.github.io/haskell4/#traversable
--
-- /Hint/: Useful function VXNlIHRyYXZlcnNl
--
-- >>> parse (string "hello") "hello bob"
-- Just (" bob","hello")
-- >>> parse (string "hey") "hello bob"
-- Nothing
string :: String -> Parser String
string = traverse is 

-- | Parse a Nothing value.
--
-- Parse the string "Nothing", if succeeds return Nothing.
--
-- >>> parse nothing "something"
-- Nothing
-- >>> parse nothing "Nothing"
-- Just ("",Nothing)
nothing :: Parser (Maybe a)
nothing = Nothing <$ string "Nothing"

-- | Parse a Just value.
--
-- Parse the string "Just ", followed by the given parser.
--
-- /Hint/: The parsed value should be wrapped with a Just
--
-- >>> parse (just int) "Just 1"
-- Just ("",Just 1)
-- >>> parse (just int) "Nothing"
-- Nothing
just :: Parser a -> Parser (Maybe a)
just p = Just <$> (string "Just " *> p)

-- | Parse a 'Maybe'
--
-- This is named parseMaybe due to a nameclash with Prelude.maybe
--
-- /Hint/: What does (<|>) do?
--
-- /Hint 2/: Maybe types can only be just or nothing
--
-- >>> parse (parseMaybe int) "Just 1"
-- Just ("",Just 1)
--
-- >>> parse (parseMaybe int) "Nothing"
-- Just ("",Nothing)
--
-- >>> parse (parseMaybe int) "Something Else"
-- Nothing
parseMaybe :: Parser a -> Parser (Maybe a)
parseMaybe p = (Just <$> (string "Just " *> p)) <|> (string "Nothing" $> Nothing)

-- | Write a parser that will parse zero or more spaces.
--
-- /Hint/: Remember the `space` parser!
-- /Hint2/: Useful function: bWFueQ==
--
-- >>> parse spaces " abc"
-- Just ("abc"," ")
--
-- >>> parse spaces "abc"
-- Just ("abc","")
spaces :: Parser String
spaces = many space

-- | Return a parser that produces one or more space chars (consuming
-- until the first non-space) but fails if:
--
--   * the input is empty; or
--   * the first produced char is not a space.
--
-- /Hint/: Useful function: c29tZQ==
-- >>> parse spaces1 " abc"
-- Just ("abc"," ")
--
-- >>> parse spaces1 "abc"
-- Nothing
spaces1 :: Parser String
spaces1 = some space
