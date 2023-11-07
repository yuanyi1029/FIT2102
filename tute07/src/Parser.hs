-- | Functions that take a string and convert it to a value. For now
-- we will only consider these functions, but in future weeks we will
-- see how these can be much more useful.
--
-- THERE IS USEFUL INFORMATION IN THE README ABOUT THE PARSING FUNCTION TYPE.
module Parser (prettifyBinaryTree) where

import BinTree (BinTree (..), pretty)

-- | Parse a single character
--
-- /Hint/: What are the ways we can pattern match on a list?
--
-- >>> char "abc"
-- Just ("bc",'a')
--
-- >>> char ""
-- Nothing
char :: String -> Maybe (String, Char)
char (c : rest) = Just (rest, c) 
char "" = Nothing

-- | Parse numbers as int until non-digit
--
-- We use the `reads` function to parse the string as an Int, and convert
-- the result to our desired type. This is mainly for convenience.
--
-- Do NOT use reads elsewhere unless explicitly approved or provided.
--
-- >>> int "123abc"
-- Just ("abc",123)
--
-- >>> int "abc"
-- Nothing
int :: String -> Maybe (String, Int)
int s = case (reads s :: [(Int, String)]) of
  [(x, rest)] -> Just (rest, x)
  _ -> Nothing

-- | Parses a specific character, otherwise return Nothing
--
-- /Hint/: What form of pattern matching can we use to values we want?
--
-- /Hint 2/: What can we use the `char` function for?
--
-- /Optional/: Use guards to simplify the validation expression.
--
-- >>> (is 'c') "cba"
-- Just ("ba",'c')
--
-- >>> (is 'c') "abc"
-- Nothing
is :: Char -> String -> Maybe (String, Char)
is c s = case char s of
  Just (rest, c1) -> if c1 == c then Just(rest, c1) else Nothing
  _ -> Nothing

data IntPair = IntPair Int Int
  deriving (Show)

-- | Parse an IntPair
--
-- >>> parseIntPair "10 20"
-- Just ("",IntPair 10 20)
--
-- >>> parseIntPair "anc10 20"
-- Nothing
parseIntPair :: String -> Maybe (String, IntPair)
parseIntPair s = case int s of
  Just (r1, x) -> case is ' ' r1 of
    Just (r2, _) -> case int r2 of
      Just (r3, y) -> Just("", IntPair x y)
      Nothing -> Nothing
    Nothing -> Nothing
  Nothing -> Nothing

-- | Parses a comma
--
-- /Hint/: What parsers have we already created?
--
-- >>> comma ",cba"
-- Just ("cba",',')
--
-- >>> comma "cba"
-- Nothing
comma :: String -> Maybe (String, Char)
comma = is ','

-- | Parse a tuple with two integers
--
-- The structure of an int 2-tuple is "(<int>,<int>)"
--
-- (This is a bit ugly but we will see how to improve this in
--  future weeks.)
--
-- /Hint/: Use the parsing functions we have just created!
--
-- /Hint 2/: The pattern is similar to parseIntPair!
--   It may be useful to copy down your answer and modify it.
--
-- >>> parseIntTuple2 "(10,2)"
-- Just ("",(10,2))
--
-- >>> parseIntTuple2 "[10,2)"
-- Nothing
parseIntTuple2 :: String -> Maybe (String, (Int, Int))
parseIntTuple2 s = case is '(' s of
  Just (r1, _) -> case int r1 of
    Just (r2, x) -> case comma r2 of
      Just (r3, _) -> case int r3 of 
        Just (r4, y) -> Just ("", (x, y))
        Nothing -> Nothing
      Nothing -> Nothing
    Nothing -> Nothing
  Nothing -> Nothing

-- | Parse a serialised BinaryTree string into a BinaryTree.
--
-- The format of the string is as follows:
--  - A Leaf is represented by "L"
--  - A Node is represented by "(<value><left><right>)"
--
-- >>> parseBinaryTree "L"
-- Just ("",Leaf)
--
-- >>> parseBinaryTree "(1LL)"
-- Just ("",Node 1 Leaf Leaf)
--
-- >>> parseBinaryTree "(1(2LL)(3LL))"
-- Just ("",Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf))
--
-- >>> parseBinaryTree "(1(2(3LL)L)(4(5LL)(6LL)))"
-- Just ("",Node 1 (Node 2 (Node 3 Leaf Leaf) Leaf) (Node 4 (Node 5 Leaf Leaf) (Node 6 Leaf Leaf)))
parseBinaryTree :: String -> Maybe (String, BinTree Int)
parseBinaryTree s = case leaf s of
  Just l -> Just l
  _ -> case node s of -- This is what allows us to parse a Leaf OR a Node
    Just t -> Just t
    _ -> Nothing

-- | Parse a Leaf
--
-- >>> leaf "L"
-- Just ("",Leaf)
--
-- >>> leaf "LL"
-- Just ("L",Leaf)
--
-- >>> leaf "(1LL)"
-- Nothing
leaf :: String -> Maybe (String, BinTree Int)
leaf s = case is 'L' s of 
  Just (rest, l) -> Just (rest, Leaf)
  Nothing -> Nothing

-- | Parse a Node
--
-- /Hint/: This will need to use parseBinaryTree! This pattern is
--  called co-recursion and is what allows us to parse this structure.
--
-- >>> node "L"
-- Nothing
--
-- >>> node "(1LL)"
-- Just ("",Node 1 Leaf Leaf)
--
-- >>> node "(1(2LL)(3LL))"
-- Just ("",Node 1 (Node 2 Leaf Leaf) (Node 3 Leaf Leaf))
node :: String -> Maybe (String, BinTree Int)

node s = case is '(' s of 
  Just (r1, _) -> case int r1 of
    Just (r2, value) -> case parseBinaryTree r2 of
      Just (r3, left) -> case parseBinaryTree r3 of
        Just (r4, right) -> case is ')' r4 of 
          Just (r5, _) -> Just (r5, Node value left right)
          Nothing -> Nothing
        Nothing -> Nothing
      Nothing -> Nothing
    Nothing -> Nothing
  Nothing -> Nothing

-- | Prettify a string representation of a binary tree.
--
-- /Hint/: Use the functions we have just created!
--
-- /Hint 2/: You can use the `pretty` function from BinTree.hs
--
-- >>> putStr $ prettifyBinaryTree "(1(2LL)(3LL))"
-- 1
-- |- 2
-- |- 3
--
-- >>> putStr $ prettifyBinaryTree "(10LL)"
-- 10
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
prettifyBinaryTree s = case parseBinaryTree s of
  Just ("", tree) -> pretty tree -- Replace the arguments with something sensible
  _ -> "Invalid input"