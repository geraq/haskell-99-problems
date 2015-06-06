module NinetyNineProblems1to10 where
-- Problem 1
-- myLast = head . reverse (or just "last")
myLast [] = error "empty list"
myLast [x] = x
myLast (x:xs) = myLast xs

-- Problem 2
-- myButLast = head . tail . reverse
-- myButLast = last . init
myButLast [] = error "empty list"
myButLast [x] = error "not enough elements"
myButLast (x:xs) = myButLast' x xs
myButLast' e [x] = e
myButLast' e (x:xs) = myButLast' x xs

-- Problem 3 (Find K-th element in 1-based list)
-- elementAt xs n = xs !! (n - 1)
-- elementAt xs n = head $ drop (n-1) xs
elementAt _ 0 = error "index is 1-based"
elementAt [] _ = error "empty list"
elementAt (x:xs) k | k == 1 = x
                   | otherwise = elementAt xs (k - 1)
                   
-- Problem 4 (length)
-- myLength = foldl (\ n _ -> n + 1) 0 (or just "length")
-- myLength [] = 0
-- myLength (x:xs) = 1 + myLength xs
myLength = myLength' 0
myLength' e [] = e
myLength' e (x:xs) = myLength' (e + 1) xs
                   
-- Problem 5 (reverse)
-- myReverse = foldl (flip (:)) [] (or just "reverse")                    
myReverse = myReverse' []
myReverse' ys [] = ys
myReverse' ys (x:xs) = myReverse' (x:ys) xs
                   
-- Problem 6 (isPalindrome)        
-- isPalindrome xs = xs == reverse xs
isPalindrome xs = isPalindrome' xs xs []
isPalindrome' xs [] zs = isPalindrome'' xs zs
isPalindrome' xs (y:ys) zs = isPalindrome' xs ys (y:zs)
isPalindrome'' [] [] = True
isPalindrome'' (x:xs) (y:ys) | x == y = isPalindrome'' xs ys
                             | otherwise = False

-- Problem 7 (flatten a nested list)
data NestedList a = Elem a | List [NestedList a]

{--
*Main> flatten (Elem 5)
[5]
*Main> flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]])
[1,2,3,4,5]
*Main> flatten (List [])
[]
--}

flatten (Elem a) = [a]
flatten (List xs) = concatMap flatten xs

-- Problem 8 (eliminate consecutive duplicates)
-- compress [] = []
-- compress (x:xs) = reverse $ foldl (\ hs e -> if e == head hs then hs else e:hs) [x] xs
compress [] = []
compress (x:xs) = x : compress' x xs
compress' _ [] = []
compress' h (x:xs) | h == x = compress' h xs
                   | otherwise = x : compress' x xs

-- Problem 9 (pack duplicates in separate lists) (it's "group")
-- pack [] = []
-- pack (x:xs) = reverse $ (uncurry (:)) $ foldl (\ (ys, hs) e -> if e == head ys then (e:ys, hs) else ([e], ys:hs)) ([x], []) xs
pack [] = []
pack (x:xs) = pack' [x] xs
pack' ys [] = [ys]
pack' ys (x:xs) | head ys == x = pack' (x:ys) xs
                | otherwise = ys : pack' [x] xs

-- Problem 10 (run-length encoding of a list) (it's "group" and "map length")

encode xs = map (\xs -> (length xs, head xs)) $ pack xs
--encode = (map (\xs -> (length xs, head xs))) . pack -- Unresolved top-level overloading??


           
                   