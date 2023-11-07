-- |
-- Some small functions that highlight useful functions in the Prelude.
--
-- Make sure to to use Hoogle https://hoogle.haskell.org/ to find functions!
--
-- None of these functions should use explicit recursion. Try to combine
-- useful small functions together.
--
-- REFER TO THE README FOR SOME USEFUL INFORMATION.
--
-- You will have to write the types for some of the functions. Polymorphism must
-- be used when possible instead of fixed types.
-- See https://tgdwyer.github.io/haskell2/#type-parameters-and-polymorphism
--
-- These functions are intended to leverage useful comparison typeclasses,
--  see https://tgdwyer.github.io/haskell2/#typeclasses
--  see https://tgdwyer.github.io/haskell3/#functor
--  see https://tgdwyer.github.io/haskell3/#applicative
module Examples (nestedMap) where

-- import Data.Function (on)
import Data.List (groupBy, sortOn)
import Prelude hiding ((<*))

-- \$setup

-- | Sort each sublist by first value in tuple
--
-- >>> sortList [[(1,"a"),(3, "b"),(2, "d")], [(5, "a"),(6, "b")]]
-- [[(1,"a"),(2,"d"),(3,"b")],[(5,"a"),(6,"b")]]
-- >>> sortList [[(Just 1,1),(Nothing, 3),(Just 5, 5)], [(Just 6, 0),(Nothing, 1)]]
-- [[(Nothing,3),(Just 1,1),(Just 5,5)],[(Nothing,1),(Just 6,0)]]
sortList :: Ord a => [[(a, b)]] -> [[(a, b)]]
sortList nlst = sortOn fst <$> nlst

-- | Group consecutive elements which are equal after applying function
--
-- >>> groupEqual id ['a', 'a', 'b', 'b', 'b', 'b', 'c','c','c','d','d','e','e']
-- ["aa","bbbb","ccc","dd","ee"]
-- >>> groupEqual (`mod` 5) [5,5,5,5,10,10,10,15,15,15]
-- [[5,5,5,5,10,10,10,15,15,15]]
--
-- /Hint/: Lookup `groupBy` which we have imported from Data.List
groupEqual :: Eq b => (a -> b) -> [a] -> [[a]]
groupEqual f = groupBy (\x y -> f x == f y) 

-- | Map a function over a Functor of Functors
--
-- /Hint/: maybe we need 2 fmaps
--
-- >>> nestedMap (+1) [[1, 2, 3], [4, 5, 6]]
-- [[2,3,4],[5,6,7]]
--
-- >>> nestedMap (+1) [Just 1, Just 2]
-- [Just 2,Just 3]
--
-- >>> nestedMap (+1) [Just 1, Nothing]
-- [Just 2,Nothing]
--
-- >>> nestedMap (+1) (Just (Just 5))
-- Just (Just 6)
--
-- >>> nestedMap (+1) (Just Nothing)
-- Just Nothing
nestedMap :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
nestedMap f nfunc = (f <$>) <$> nfunc

-- | Takes an unary function and applies it to one element wrapped in a context.
--
-- >>> liftA (+1) (Just 7)
-- Just 8
liftA :: Applicative f => (a -> b) -> f a -> f b
liftA f a = f <$> a

-- | Takes a binary function and applies it to two elements wrapped in a context.
--
-- >>> liftA2 (+) [1, 2, 3] [4, 5]
-- [5,6,6,7,7,8]
--
-- >>> liftA2 (+) (Just 7) (Just 8)
-- Just 15
--
-- >>> liftA2 (+) (Just 7) Nothing
-- Nothing
--
-- >>> liftA2 (+) Nothing (Just 8)
-- Nothing
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f a b = f <$> a <*> b

-- | Implement a version of the applicative that combines the effects of boths sides, but retains only the value of the left-hand side
--
-- The "effect" of a Maybe is to either succeed with a Just or fail with a Nothing.
-- Both sides succeed, return the left value:
-- >>> Just 5 <* Just 2
-- Just 5
--
-- Left side fails so the effect is Nothing
-- >>> Nothing <* Just 2
-- Nothing
--
-- right side fails so the effect is Nothing
-- >>> Just 2 <* Nothing
-- Nothing
--
-- The "effect" of the list applicative is to apply all functions in the list on the left to all values in the list on the right.
-- The <* operator creates a function for each value on the left which just returns that value, regardless of what it is applied to,
-- and applies that function to each value in the list on the right.
-- Therefore, <* gives us the list on the left, but with a multiplicity of the cartesian product of the two lists:
-- >>> [4,5] <* [1,2,3]
-- [4,4,4,5,5,5]
--
-- /Hint/: you'll need to lift a binary function that returns the first argument regardless of its second argument into the applicative context
(<*) :: Applicative f => f a -> f b -> f a
(<*) fa fb = const <$> fa <*> fb
