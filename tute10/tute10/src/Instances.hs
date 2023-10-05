{-# LANGUAGE InstanceSigs #-}
module Instances where
import Control.Applicative (Alternative (empty, (<|>)))

import Data.Char
  ( isSpace,
    isUpper,
  )

-- $setup
-- >>> let p = \n -> P (\x -> Result x n)
-- >>> let add = \n -> \m -> P (\x -> Result x (n+m))

data ParseError
  = UnexpectedEof
  | ExpectedEof Input
  | UnexpectedChar Char
  | UnexpectedString String
  deriving (Eq, Show)

data ParseResult a
  = Error ParseError
  | Result Input a
  deriving (Eq)

type Input = String

newtype Parser a = P {parse :: Input -> ParseResult a}

-- Result Instances
instance Show a => Show (ParseResult a) where
  show (Result i a) = "Result >" ++ i ++ "< " ++ show a
  show (Error UnexpectedEof) = "Unexpected end of stream"
  show (Error (UnexpectedChar c)) = "Unexpected character: " ++ show [c]
  show (Error (UnexpectedString s)) = "Unexpected string: " ++ show s
  show (Error (ExpectedEof i)) =
    "Expected end of stream, but got >" ++ show i ++ "<"

instance Functor ParseResult where
  fmap :: (a -> b) -> ParseResult a -> ParseResult b
  fmap f (Result i a) = Result i (f a)
  fmap _ (Error e) = Error e


-- | Implement this before Applicative and Functor Parser!
-- Do not use <$> or <*> for this, they are not implemented (yet)
-- The monadic instance should 
-- >>> parse (  p 1 >>= add 2) ""
-- Result >< 3
--
-- prop> \i j -> parse (p i >>= add j) "" == Result "" (i + j)
instance Monad Parser where
  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  (>>=) (P p) f = undefined 

-- Parser Instances

-- | Functor instance for a parser
-- | Rewrite this using bind (>>=)
-- >>> parse ((+1) <$> P (`Result` 1)) "hello"
-- Result >hello< 2

--  >>> parse ((+1) <$> P (const (Error UnexpectedEof))) "hello"
-- Unexpected end of stream
instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f pa = undefined

  -- fmap f (P p) = P $ \i -> case p i of
  --   Result rest x -> parse (pure (f x)) rest
  --   Error e -> Error e

-- | Write (<*>) using bind!
--
-- >>> parse (P (`Result` (+1)) <*> P (`Result` 1)) "hello"
-- Result >hello< 2
--
-- >>> parse (pure (+1) <*> P (`Result` 1)) "hello"
-- Result >hello< 2
--
-- >>> parse (pure (+1) <*> (pure 1 :: Parser Int)) "hello"
-- Result >hello< 2
instance Applicative Parser where
  -- creates a Parser that always succeeds with the given input
  pure :: a -> Parser a
  pure = P . flip Result

  -- Write this using bind
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  (<*>) p q = undefined

  -- (<*>) (P p) q = P $ \i -> case p i of
  --   Result rest f -> parse (f <$> q) rest
  --   Error e -> Error e

instance Alternative Parser where
  empty :: Parser a
  empty = P (\i -> Error UnexpectedEof)

  (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) (P p) (P q) = P $ \i -> case p i of
    r@(Result _ _) -> r
    _ -> q i