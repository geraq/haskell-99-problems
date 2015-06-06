module NinetyNineProblems54Ato60 where

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)
leaf x = Branch x Empty Empty

-- Problem 54A
{-
- check in an expression is a valid binary tree
- redundant in Haskell, every valid expression will have a valid type,
so it will always be true.
-}

isTree Empty = True 
isTree (Branch _ _ _) = True

