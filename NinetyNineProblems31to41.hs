module NinetyNineProblems31to41 where
import Data.List

-- Problem 31
{-
P31> isPrime 7
True
-}

isPrime n = length (filter ((==0).(n `mod`)) [1..n]) == 2

-- Problem 32
{-
[myGCD 36 63, myGCD (-3) (-6), myGCD (-3) 6]
[9,3,3]
-}
		  
myGCD a b | b == 0 = abs a
		  | otherwise = myGCD b (a `mod` b)
		  
-- Problem 33
{-
* coprime 35 64
True
-}		  
		  
coprime a b = myGCD a b == 1		  
		  
-- Problem 34
{-
* (totient-phi 10)
4
-}
	
totient 1 = 1	
totient n = length $ filter (coprime n) [1..n-1]		  
		  
-- Problem 35		  
{-
> primeFactors 315
[3, 3, 5, 7]
-}		  
		  
primeFactors n = primeFactors' n 2
primeFactors' n d | n == 1 = []				  
				  | n `mod` d == 0 = d : primeFactors' (n `div` d) d		  
				  | otherwise = primeFactors' n (d + 1)
		  
-- Problem 36
{-
*Main> prime_factors_mult 315
[(3,2),(5,1),(7,1)]
-}		  
		  
prime_factors_mult = map (\xs -> (head xs, length xs)) . group . primeFactors		  
		  
-- Problem 37
{-
phi(m) = (p1 - 1) * p1 ** (m1 - 1) * 
         (p2 - 1) * p2 ** (m2 - 1) * 
         (p3 - 1) * p3 ** (m3 - 1) * ...
-}		  
		  
phi m = foldl (\t (e,n) -> t * ((e - 1) * e ^ (n - 1))) 1 $ prime_factors_mult m		  
		  
-- Problem 38
{-
compare totient (P34) vs phi (P37)
No solution required

totient: (5637361 reductions, 8024073 cells, 8 garbage collections)
phi: (85117 reductions, 122668 cells)

phi is much more efficient than totient
-}
		  
-- Problem 39
{-
P29> primesR 10 20
[11,13,17,19]
-}		  
		  
primesR a b = filter isPrime [a..b]		  
		  
-- Problem 40
{-
*goldbach 28
(5, 23)
-}
		  
goldbach n | n <= 2 || odd n = error "n must be even and greater than 2"		  
		   | otherwise = head $ filter (\(a,b) -> isPrime a && isPrime b) $ map (\x -> (x, n - x)) [2..n-1] 
		  
-- Problem 41
{-
*Exercises> goldbachList 9 20
[(3,7),(5,7),(3,11),(3,13),(5,13),(3,17)]
*Exercises> goldbachList' 4 2000 50
[(73,919),(61,1321),(67,1789),(61,1867)]
-}		  

--print with format
{-
goldbachList a b = do
					  let tuples = map (\x -> let (n,m) = goldbach x in (x,n,m)) $ filter even [a..b];
						  output = map (\(x,n,m) -> show x ++ " = " ++ show n ++ " + " ++ show m) tuples;
					  putStrLn (tail $ concatMap ('\n':) output)
					  
goldbachList' a b v = do
					  let tuples = filter (\(_, n, m) -> n > v && m > v) $ map (\x -> let (n,m) = goldbach x in (x,n,m)) $ filter even [a..b];
						  output = map (\(x,n,m) -> show x ++ " = " ++ show n ++ " + " ++ show m) tuples;
					  putStrLn (tail $ concatMap ('\n':) output)		  
-}		  

-- do not print
goldbachList a b = map goldbach $ filter even [a..b]						  					
					  
goldbachList' a b v = filter (\(n, m) -> n > v && m > v) $ goldbachList a b





