module NinetyNineProblems46to50 where
import Data.List

-- Problem 46
{-
> table (\a b -> (and' a (or' a b)))
True True True
True False True
False True False
False False False
-}

and' x y | x == False = False
         | otherwise = y

or' x y | x == True = True
        | otherwise = x
         
nand' x y | x == False = True
           | otherwise = not y

nor' x y | x == True = False
         | otherwise = not x         
         
xor' x y | x /= y = True
         | otherwise = False
         
impl' x y = or' (not x) y         
         
equ' x y | x == y = True
         | otherwise = False         
         
table f = do
                let rows = [(x, y, f x y) | x <- [True, False], y <- [True, False]];
                    output = map (\(a,b,c) -> unwords [show a, show b, show c]) rows;
                putStrLn (tail $ concatMap ('\n':) output)
         
-- Problem 47
{-
> table2 (\a b -> a `and'` (a `or'` not b))
True True True
True False True
False True False
False False False

Same solution as before but used as infix operators
-}         
         
-- Problem 48
{-
> tablen 3 (\[a,b,c] -> a `and'` (b `or'` c) `equ'` a `and'` b `or'` a `and'` c)
-- infixl 3 `equ'`
True  True  True  True
True  True  False True
True  False True  True
True  False False True
False True  True  True
False True  False True
False False True  True
False False False True
 
-- infixl 7 `equ'`
True  True  True  True
True  True  False True
True  False True  True
True  False False False
False True  True  False
False True  False False
False False True  False
False False False False

tablen 3 (\[a,b,c] -> (a `and'` (b `or'` c)) `equ'` ((a `and'` b) `or'` (a `and'` c)))

-}         
         
tablen n f = do
                let rows = map (\xs -> (xs, f xs)) $ makeRows n;
                    output = map (\(xs, r) -> unwords $ map show (xs ++ [r])) rows;
                    makeRows 1 = [[True], [False]];
                    makeRows n = concatMap (\ xs -> map (\x -> xs++[x]) [True, False]) $ makeRows (n-1)                    
                putStrLn (tail $ concatMap ('\n':) output)         
         
-- Problem 49
{-
Gray codes
P49> gray 3
["000","001","011","010","110","111","101","100"]
-}         
         
gray 1 = ["0","1"]
gray n = let xs = gray (n-1)         
         in (map ('0':) xs) ++ (map ('1':) (reverse xs))
         
-- Problem 50
{-
Huffman codes
*Exercises> huffman [('a',45),('b',13),('c',12),('d',16),('e',9),('f',5)]
[('a',"0"),('b',"101"),('c',"100"),('d',"111"),('e',"1101"),('f',"1100")]
-}
         
-- Naive direct implementation, build bottom-up binary tree
         
data HuffmanTree a = Leaf Int a | Node Int (HuffmanTree a) (HuffmanTree a) deriving Show
getVal (Leaf v _) = v         
getVal (Node v _ _) = v        
         
huffman xs = reSort xs $ assignCodes "" [] $ makeTree $ map (\(v,f) -> Leaf f v) xs          
makeTree [n] = n
makeTree ns = let ss = sortBy (\n m -> compare (getVal n) (getVal m)) ns; 
                  (n1, n2) = (\[a,b] -> (a,b)) $ take 2 ss;
                  newNode = (Node (getVal n1 + getVal n2) n1 n2);
              in makeTree (newNode: drop 2 ss)
assignCodes str rs (Leaf _ v) = (v, reverse str):rs
assignCodes str rs (Node _ hl hr) = assignCodes ('0':str) (assignCodes ('1':str) rs hr) hl
reSort xs rs = map (\(x,_) -> head $ filter ((==x). fst) rs) xs
         
 