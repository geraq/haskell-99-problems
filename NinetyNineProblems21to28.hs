module NinetyNineProblems21to28 where
import System.Random
import Data.List

-- Problem 21
{-
P21> insertAt 'X' "abcd" 2
"aXbcd"
-}

insertAt e xs n = take (n - 1) xs ++ [e] ++ drop (n - 1) xs

-- Problem 22
{-
Prelude> range 4 9
[4,5,6,7,8,9]
-}

range i j | i > j = []
          | i == j = [j]
          | otherwise = i : range (i + 1) j

-- Problem 23
{-
Prelude System.Random>rnd_select "abcdefgh" 3 >>= putStrLn
eda
-}

rnd_select xs n = do 
                    let m = length xs;
                    indexes <- sequence (take n $ map (\x -> randomRIO (0,x) :: IO Int) [m-1, m-2..0])                    
                    let output = reverse $ fst $ foldl (\ (rs, es) i -> let (e, rest) = takeElement i es in (e:rs, rest)) ([], xs) indexes;
                        takeElement i es = (es !! i, map snd $ filter ((/= i) . fst) $ zip [0..] es);                            
                    print output
                    
-- Problem 24
{-
Prelude System.Random>diff_select 6 49
Prelude System.Random>[23,1,17,33,21,37]
-}

diff_select n m = rnd_select [1..m] n

{- Problem 25
Prelude System.Random>rnd_permu "abcdef"
Prelude System.Random>"badcef"
-}

permu xs = rnd_select xs (length xs)

{- Problem 26
> combinations 3 "abcdef"
["abc","abd","abe",...]
-}

combinations _ [] = []
combinations 1 xs = map (\x -> [x]) xs
combinations n (x:xs) = (map (x:) (combinations (n-1) xs)) ++ combinations n xs

-- Problem 27
{-
P27> group [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
[[["aldo","beat"],["carla","david","evi"],["flip","gary","hugo","ida"]],...]
(altogether 1260 solutions)
 
27> group [2,2,5] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
[[["aldo","beat"],["carla","david"],["evi","flip","gary","hugo","ida"]],...]
(altogether 756 solutions)
-}

{-
myGroup [2,3,4] ["aldo","beat","carla","david","evi","flip","gary","hugo","ida"]
-}

myGroup [] xs = []
myGroup [n] xs = map (\ comb -> [comb]) $ combinations n xs
myGroup (n:ns) xs = let combs = combinations n xs;
                        remainders = map (\ls -> filter (\ x -> not $ elem x ls) xs) combs;
                        pairs = zip combs remainders                    
                    in concatMap (\(comb, remainder) -> map (comb :) (myGroup ns remainder)) pairs                    
                    
printGroup ns xs = do
                        let rs = myGroup ns xs;
                            output = map (('\n':) . show) rs;
                            path = "combinations_group.txt"
                        --putStrLn (tail $ concat output)
                        writeFile path (tail $ concat output)
-- Problem 28
{-
a)
Prelude>lsort ["abc","de","fgh","de","ijkl","mn","o"]
Prelude>["o","de","de","mn","abc","fgh","ijkl"]

b)
lfsort ["abc", "de", "fgh", "de", "ijkl", "mn", "o"]
["ijkl","o","abc","fgh","de","de","mn"]
-}

lsort xss = map snd $ sortBy (\(l1,_) (l2,_) -> compare l1 l2) $ map (\ xs -> (length xs, xs)) xss

lfsort xss = map (\(_,_,xs) -> xs) $ sortBy (\(lf1,_,_) (lf2,_,_) -> compare lf1 lf2) $ lenFreqs
                where input = map (\ xs -> (length xs, xs)) xss;                      
                      lenFreqs = map (\(l,xs) -> (length $ filter ((==l) . fst) input, l, xs)) input

                      
                      
                      
                      