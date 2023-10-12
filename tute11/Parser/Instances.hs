{-# LANGUAGE InstanceSigs #-}

module Instances (Parser (..), int, float) where

-- You may add more imports as you wish/need
import Control.Applicative (Alternative (empty, (<|>)))

newtype Parser a = Parser {parse :: String -> Maybe (String, a)}

-- | Applies the mapping function to the *result* (parsed value) of the parser.
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) = Parser (((f <$>) <$>) <$> p)

-- | Similar to Functor, we want to apply the function to the parsed value.
instance Applicative Parser where
  pure :: a -> Parser a
  pure a = Parser (\b -> Just (b, a))

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p1 <*> p2 = Parser $ \i -> case parse p1 i of
    Just (rest, f) -> parse (f <$> p2) rest
    Nothing -> Nothing

-- | 'Alternative' is used for parsers that support choice (alternative) and
-- failure (empty). In the context of this 'Parser' type:
--
-- 1. 'empty' represents a parser that always fails
--
-- 2. '<|>' combines two parsers. It tries the first parser on the input,
-- and if it fails it tries the second parser.
instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ const Nothing

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (Parser pa) (Parser pb) = Parser $ \x -> pa x <|> pb x

-- | Uses Text.Read.reads to parse a value.
--
-- Must NOT be used unless provided or explicitly approved.
readParser :: Read a => Parser a
readParser = Parser f
  where
    f s = case reads s of
      [(x, rest)] -> Just (rest, x)
      _ -> Nothing

-- | Parse input as int until non-digit
int :: Parser Int
int = readParser

-- | Parse input as float until non-digit
--
-- >>> parse float "12.3abc"
-- Just ("abc",12.3)
--
-- >>> parse float "-12.3a"
-- Just ("a",-12.3)
--
-- >>> parse float "--12.3a"
-- Nothing
--
-- >>> parse float "abc"
-- Nothing
float :: Parser Float
float = readParser
