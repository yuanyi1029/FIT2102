-- | Implementation of integer valued binary trees.
--
-- The focus of these exercises is to get comfortable with a more
-- complex custom type, and using recursion.
module BinTree () where

-- | A BinTree is a type of tree which has two children: a left and a right.
--
-- The children can be either
-- - a Leaf with no value, or
-- - another BinTree
--
-- See https://tgdwyer.github.io/haskell2/#algebraic-data-types
data BinTree a = Leaf | Node a (BinTree a) (BinTree a)
  deriving (Show)

-- $setup
-- >>> tree = Node 16 (Node 23 Leaf (Node 73 Leaf Leaf)) (Node 42 Leaf Leaf) :: BinTree Int
-- >>> one = Node 1 Leaf Leaf :: BinTree Int

-- | Find the number of nodes in a tree.
--
-- See https://tgdwyer.github.io/haskell2/#pattern-matching
--
-- >>> size Leaf
-- 0
--
-- >>> size one
-- 1
--
-- >>> size tree
-- 4
size :: BinTree a -> Int
size Leaf = 0
size (Node _ left right) = 1 + size left + size right

-- | Find the depth of a tree (number of levels)
--
-- See https://tgdwyer.github.io/haskell2/#pattern-matching
--
-- >>> depth Leaf
-- 0
--
-- >>> depth (Node 1 Leaf Leaf)
-- 1
--
-- >>> depth tree
-- 3
depth :: BinTree a -> Int
depth Leaf = 0
depth (Node _ left right) = 1 + max (depth left) (depth right)

-- | Map a function over a tree.
--
-- >>> mapTree (+1) Leaf
-- Leaf
--
-- >>> mapTree (*1) one
-- Node 1 Leaf Leaf
--
-- >>> mapTree (`mod` 2) tree
-- Node 0 (Node 1 Leaf (Node 1 Leaf Leaf)) (Node 0 Leaf Leaf)
mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree _ Leaf = Leaf
mapTree f (Node value left right) = Node (f value) (mapTree f left) (mapTree f right)

-- | -----------------------
-- |  SUPPLEMENTARY CONTENT
-- | -----------------------

-- | Reduce a tree to a single value.
--
-- The accumulator function (b -> a -> b) takes the
-- accumulated value as the first argument, and the current
-- node value as the second argument.
--
-- (Optional) The order of the reduce is left to right. That is, the
-- the current node is accumulated first, then the left subtree is reduced,
-- and then the right subtree.
--
-- /Challenge/: Can we implement depth using foldTree? Why or why not?
--
-- >>> foldTree (+) 0 tree
-- 154
--
-- >>> foldTree (min) maxBound tree
-- 16
--
-- >>> foldTree (max) minBound tree
-- 73
--
-- >>> foldTree (flip (-)) 0 tree
-- -24
foldTree :: (b -> a -> b) -> b -> BinTree a -> b
foldTree = undefined
