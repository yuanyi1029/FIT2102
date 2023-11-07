-- | !! SUPPLEMENTARY MATERIAL !!
--
-- Reimplement the binary tree parsing and prettifying from last week,
-- this time using Functor and Applicative.
--
-- You can also run this using `stack run`. Refer to the README.
module BinTree (prettifyBinaryTree) where

import Control.Applicative (Alternative ((<|>)), liftA3)
import Data.Functor (($>))
import Examples (nestedMap)
import Parser (Parser, close, int, is, open, parse)

data BinTree a = Leaf | Node a (BinTree a) (BinTree a)
  deriving (Show)

-- | Parse a serialised BinaryTree string into a BinaryTree.
--
-- The format of the string is as follows:
--  - A Leaf is represented by "L"
--  - A Node is represented by "(<value><left><right>)"
--
-- >>> parse binaryTree "L"
-- Just ("",Leaf)
--
-- >>> parse binaryTree "(1LL)"
-- Just ("",Node 1 Leaf Leaf)
--
-- >>> parse binaryTree "(1(2LL)(3LL))"
-- Just ("",Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf))
--
-- >>> tree = Node 1 (Node 2 (Node 3 Leaf Leaf) Leaf) (Node 4 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf))
-- >>> parse binaryTree "(1(2(3LL)L)(4(5LL)(6LL)))"
-- Just ("",Node 1 (Node 2 (Node 3 Leaf Leaf) Leaf) (Node 4 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf)))
binaryTree :: Parser (BinTree Int)
binaryTree = undefined

-- | Parse a Leaf
--
-- >>> parse leaf "L"
-- Just ("",Leaf)
--
-- >>> parse leaf "LL"
-- Just ("L",Leaf)
--
-- >>> parse leaf "(1LL)"
-- Nothing
leaf :: Parser (BinTree Int)
leaf = undefined

-- leaf s = case is 'L' s of 
--   Just (rest, l) -> Just (rest, Leaf)
--   Nothing -> Nothing

-- | Parse a Node
--
-- /Hint/: How do we construct a Node?
--  We need to parse the value, left subtree, right subtree, and then
--  combine the values together. But they are in different contexts!
--
-- >>> parse node "L"
-- Nothing
--
-- >>> parse node "(1LL)"
-- Just ("",Node 1 Leaf Leaf)
--
-- >>> parse node "(1(2LL)(3LL))"
-- Just ("",Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf))
node :: Parser (BinTree Int)
node = undefined

-- | Prettify a string representation of a binary tree.
--
-- /Hint/: Use the functions and parsers we have already created!
--
-- >>> putStr $ prettifyBinaryTree "(1L(2(3LL)L))"
-- 1
-- |- 2
-- |- |- 3
--
-- >>> putStr $ prettifyBinaryTree "(1(2(3LL)L)(4(5LL)(6LL)))"
-- 1
-- |- 2
-- |- |- 3
-- |- 4
-- |- |- 5
-- |- |- 6
prettifyBinaryTree :: String -> String
prettifyBinaryTree s = case undefined of
  Just (_, t) -> t
  _ -> "Invalid input"

-- | Prettify a binary tree
pretty :: Show a => BinTree a -> String
pretty n = pretty' n ""
  where
    pretty' :: Show a => BinTree a -> String -> String
    pretty' Leaf _ = ""
    pretty' (Node v l r) p = p ++ show v ++ "\n" ++ pretty' l (p ++ "|- ") ++ pretty' r (p ++ "|- ")
