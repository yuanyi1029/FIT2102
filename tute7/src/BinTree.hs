-- | Implementation of integer valued binary trees.
--
-- Some more practise with recursion and working with Haskell syntax.
module BinTree (BinTree (..), pretty) where

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
-- >>> one = Node 1 Leaf Leaf :: BinTree Int
-- >>> tree = Node 16 (Node 23 Leaf (Node 73 Leaf Leaf)) (Node 42 Leaf Leaf) :: BinTree Int
-- >>> bigTree = (Node 1 (Node 2 (Node 3 Leaf Leaf) Leaf) (Node 4 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf))) :: BinTree Int

-- | Pretty print a binary tree.
--
-- /Hint/: An auxiliary function is useful here.
--
-- /Hint 2/: Have a look at the week 2 solution below, you can use that as reference!
--
-- /Challenge/: Write this using foldTree and mapTree from Week 6.
--   Is this better than the recursive implementation?
--
-- >>> putStr $ pretty Leaf
--
--
-- >>> putStr $ pretty one
-- 1
--
-- >>> putStr $ pretty tree
-- 16
-- |- 23
-- |- |- 73
-- |- 42
--
-- >>> putStr $ pretty bigTree
-- 1
-- |- 2
-- |- |- 3
-- |- 4
-- |- |- 5
-- |- |- 6

pretty :: Show a => BinTree a -> String
pretty tree = _pretty tree "|- "

_pretty :: Show a => BinTree a -> String -> String
_pretty Leaf _ = ""
_pretty (Node value left right) prefix = 
  show value ++ 
  (case _pretty left (prefix ++ "|- ") of
    "" -> ""
    leftValue -> "\n" ++ prefix ++ leftValue) ++ 
  (case _pretty right (prefix ++ "|- ") of
    "" -> ""
    rightValue -> "\n" ++ prefix ++ rightValue)  
  
{-

function prettifyBinaryTree<T extends Object>(node: BinaryTree<T>): string {
  function _prettifyBinaryTree(node: BinaryTree<T>, prefix: string): string {
    const current = node.data.toString();

    const left = node.left
      ? "\n" + prefix + _prettifyBinaryTree(node.left, prefix + "|- ")
      : "";
    const right = node.right
      ? "\n" + prefix + _prettifyBinaryTree(node.right, prefix + "|- ")
      : "";

    return [current, left, right].join("");
  }

  return _prettifyBinaryTree(node, "|- ");
}

-}
