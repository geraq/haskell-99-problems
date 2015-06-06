module NinetyNineProblems11to20 where
import NinetyNineProblems1to10

-- Problem 11 (modified run-length encoding)
{-
P11> encodeModified "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
Multiple 2 'a',Single 'd',Multiple 4 'e']
-}

data Element a = Single a | Multiple Int a deriving Show

encodeModified xs = map transform $ encode xs
transform (n, e) | n == 1 = Single e
			     | otherwise = Multiple n e

-- Problem 12 (decode run-length list)
{-P12> decodeModified  [Multiple 4 'a',Single 'b',Multiple 2 'c', Multiple 2 'a',Single 'd',Multiple 4 'e']
"aaaabccaadeeee"
-}

decodeModified = concatMap decode'
				 where decode' (Single e) = [e];
					   decode' (Multiple n e) = replicate n e
					
-- Problem 13 (Run-length encoding of a list (direct solution).)
-- Same as 11 ?????
{-
P13> encodeDirect "aaaabccaadeeee"
[Multiple 4 'a',Single 'b',Multiple 2 'c',
 Multiple 2 'a',Single 'd',Multiple 4 'e']
-}
encodeDirect (x:xs) = map transform $ reverse $ (uncurry (:)) $ foldl (\ ((n, h), hs) e -> if e == h then ((n + 1, h), hs) else ((1, e), (n, h):hs)) ((1, x), []) xs

-- Problem 14 (duplicate elements)
{-
> dupli [1, 2, 3]
[1,1,2,2,3,3]
-}

dupli = concatMap (replicate 2)

-- Problem 15
{-
> repli "abc" 3
"aaabbbccc"
-}

repli xs n = concatMap (replicate n) xs

-- Problem 16
{-
*Main> dropEvery "abcdefghik" 3
"abdeghk"
-}

dropEvery xs n = concatMap (\ (e, i) -> if i `mod` n == 0 then [] else [e]) $ zip xs [1..]

-- Problem 17
{-
*Main> split "abcdefghik" 3
("abc", "defghik")
Do not use any predefined predicates, i.e. split xs n = (take n xs, drop n xs)
-}
split xs n = split' [] xs n
split' xs ys 0 = (reverse xs, ys)
split' xs [] _ = (reverse xs, [])
split' xs (y:ys) n = split' (y:xs) ys (n - 1)

-- Problem 18
{-
*Main> slice ['a','b','c','d','e','f','g','h','i','k'] 3 7
"cdefg"
-}

slice xs i j = take (j - i + 1) $ drop (i - 1) xs

-- Problem 19
{-
*Main> rotate ['a','b','c','d','e','f','g','h'] 3
"defghabc"
 
*Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
"ghabcdef"
-}
rotate xs n | n > 0 = drop n xs ++ take n xs
			| n < 0 = drop (t + n) xs ++ take (t + n) xs
			| otherwise = xs
			where t = length xs
			
-- Problem 20
{-
*Main> removeAt 2 "abcd"
('b',"acd")
-}			
			
removeAt n xs = (\ xs -> (fst . head . filter ((==n) . snd) $ xs, map fst $ filter ((/=n) . snd) $ xs)) $ zip xs [1..]
