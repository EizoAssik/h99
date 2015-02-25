module H99.List where

import Prelude hiding ( last
                      , elementAt
                      , splitAt
                      , reverse
                      , length ) 

import qualified Prelude as P
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.List (sortBy, findIndices, nub)
import qualified Data.List as List
import Control.Monad
import Control.Applicative
import System.Random


emptyError = error "empty list"

oorError = error "out of range"

-- Problem 1: Find the last element of a list.

last :: [a] -> a
last [] = emptyError
last (x:[]) = x
last (x:xs) = last xs

-- Problem 2: Find the last but one element of a list.

butLast :: [a] -> a
butLast [] = emptyError
butLast (x:y:[]) = x
butLast (x:[]) = oorError
butLast (x:xs) = butLast xs

-- Problem 3: Find the K'th element of a list.
-- The first element in the list is number 1.

elementAt :: [a] -> Int -> a
elementAt [] _ = oorError
elementAt (x:xs) 1 = x
elementAt (x:xs) index = 
    if index > 1
       then elementAt xs $ index - 1
       else oorError

-- Problem 4: Find the number of elements of a lis

length :: [a] -> Int
length = foldl (const . (+1)) 0

-- Problem 5: Reverse a list.

reverse :: [a] -> [a]
reverse = flip (:) `foldl` []

-- Problem 6: Find out whether a list is a palindrome.

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome = (==) <*> P.reverse

-- Problem 7: Flatten a nested list structure.

data NestedList a = Elem a | List [NestedList a]

flatten (Elem a) = [a]
flatten (List xs) = concatMap flatten xs

-- Problem 8: Eliminate consecutive duplicates of list elements.

compress :: (Eq a) => [a] -> [a]
compress [] = []
compress (x:xs@(y:ys)) =
    if x == y
       then compress xs
       else x : compress xs
compress (x:[]) = [x]

-- Problem 9: Pack consecutive duplicates of list elements into sublists.

pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = P.reverse $ snd $ foldl op (x, [[x]]) xs
    where op (x, l) c =
            if x == c
               then (x, (x:(head l)) :  (tail l))
               else (c, [c]:l)

-- and a lazy solution

packLazy :: (Eq a) => [a] -> [[a]]
packLazy [] = []
packLazy (x:xs) = packLazy' [x] x xs
    where packLazy' l c (x:xs) = 
              if c == x
                 then packLazy' (x:l) c xs
                 else l : packLazy' [x] x xs 
          packLazy' l c [] = [l]
     
-- Problem 10: Run-length encoding of a list.

encode :: (Eq a) => [a] -> [(Int, a)]
encode [] = []
encode (x:xs) = gt $ foldl op ([], x, 1) xs
    where op (l, x, n) c =
            if x == c
               then (l, x, 1 + n)
               else ((n, x):l, c, 1)
          gt (l, x, n) = P.reverse $ (n, x):l

-- and a lazy one

encodeLazy :: (Eq a) => [a] -> [(Int, a)]
encodeLazy [] = []
encodeLazy (x:xs) = encode' x 1 xs
    where encode' c n (x:xs) =
            if c == x
               then encode' c (n + 1) xs
               else (n, c) : encode' x 1 xs
          encode' c n [] = [(n, c)]

-- or, as shown on wiki, use the Problem 9

encode' :: (Eq a) => [a] -> [(Int, a)]
encode' = map ((,) <$> length <*> head) . List.group

-- Problem 11:  Modified run-length encoding.

data Encode a = Single a | Multiple Int a 
                deriving (Eq, Show)

fromPair (1, x) = Single x
fromPair (n, x) = Multiple n x

encodeModified :: (Eq a) => [a] -> [Encode a]
encodeModified = map fromPair . encodeLazy

-- Problem 12: Decode a run-length encoded list.

decodeModified = concatMap decode
    where decode (Single a) = [a]
          decode (Multiple n a) = replicate n a

-- Problem 13: Run-length encoding of a list (direct solution).

-- just like encodeLazy

encodeDirect :: (Eq a) => [a] -> [Encode a]
encodeDirect (x:xs) = encode' x 1 xs
    where encode' c n (x:xs) =
            if c == x
               then encode' c (n + 1) xs
               else consE n c : encode' x 1 xs
          encode' c n [] = [consE n c]
          consE 1 x = Single x
          consE n x = Multiple n x

-- Problem 14: Duplicate the elements of a list.

dupli :: [a] -> [a]
dupli = concatMap (replicate 2)

-- Problem 15: Replicate the elements of a list a given number of times.

repli :: Int -> [a] -> [a]
repli n xs = concatMap (replicate n) xs

-- Problem 16: Drop every N'th element from a list.

dropEvery :: [a] -> Int -> [a]
dropEvery [] n = []
dropEvery xs n =
    if n < 2 then [] else dropEvery' xs n
    where dropEvery' [] _ = []
          dropEvery' (x:xs) 1 = dropEvery' xs n
          dropEvery' (x:xs) n = x : dropEvery' xs (n - 1)

-- Problem 17: Split a list into two parts; the length of the first part is given.

splitAt :: [a] -> Int -> ([a], [a])
splitAt xs n = splitAt' [] xs n
    where splitAt' l [] _ = (P.reverse l, [])
          splitAt' l xs 0 = (P.reverse l, xs)
          splitAt' l (x:xs) n = splitAt' (x:l) xs (n - 1)

-- Problem 18: Extract a slice from a list.

slice :: [a] -> Int -> Int -> [a]
slice [] i j = []
slice xs 1 0 = []
slice (x:xs) 1 j = x : slice xs 1 (j - 1)
slice (x:xs) i j = slice xs (i - 1) (j - 1)

-- Problem 19: Rotate a list N places to the left.

rotate :: [a] -> Int -> [a]
rotate xs n = (++) <$> snd <*> fst $ splitAt xs (n `mod` P.length xs)

-- Problem 20: Remove the K'th element from a list.

removeAt :: Int -> [a] -> (a, [a])
removeAt k xs = removeAt' [] k xs

removeAt' l k [] = error "out of range"
removeAt' l k (x:xs)
    | k <  1 = error "non-positive index"
    | k == 1 = (x, (P.reverse l ++ xs))
    | otherwise = removeAt' (x:l) (k - 1) xs

-- Problem 21: Insert an element at a given position into a list.

insertAt x [] k = [x]
insertAt x l@(y:ys) k
    | k <= 1 = x : l
    | k >  1 = y : insertAt x ys (k - 1)

-- Problem 22: Create a list containing all integers within a given range.

-- thats range a b = [a..b]!

range :: Int -> Int -> [Int]
range a b =
    if a == b
       then [a]
       else a : range (a + 1) b

-- Problem 23: Extract a given number of randomly selected elements from a list.

randSelect :: [a] -> Int -> IO [a]
randSelect [] k = return []
randSelect l@(x:xs) k = do
    let len = P.length l
    if len <= k
       then return l
       else do
         v <- randomRIO (1, len)
         if v <= k
            then (x:) `fmap` randSelect xs (k-1)
            else randSelect xs k 

-- Problem 25: Generate a random permutation of the elements of a list.

diffSelect :: Int -> Int -> IO [Int]
diffSelect n m 
    | n <= 0 = return []
    | n < m && m > 1 = diffSelect' IntSet.empty n m
    | otherwise =
        error $    "the given range cannot provide "
                ++ show n
                ++ " different random numbers"

diffSelect' :: (IntSet.IntSet) -> Int -> Int -> IO [Int]
diffSelect' xset 0 m = return $ IntSet.toList xset
diffSelect' xset n m = do
    let size = IntSet.size xset
    sub <- take n . randomRs (1, m) <$> getStdGen
    let xset' = foldl (flip IntSet.insert) xset sub
        size' = n + size - IntSet.size xset'
    diffSelect' xset' size' m

-- but this runs VERY slow. Below is the wiki version

diffSelect'' :: Int -> Int -> IO [Int]
diffSelect'' n m = take n . nub . randomRs (1, m) <$> getStdGen

-- or use Problem 23

diffSelect''' :: Int -> Int -> IO [Int]
diffSelect''' n m = randSelect [1..m] n

-- Problem 26: Generate the combinations of K distinct objects
-- chosen from the N elements of a list.

combinations :: Int -> [a] -> [[a]]
combinations n [] = []
combinations 1 xs = map return xs
combinations n l@(x:xs)
    | n <= 0 = []
    | otherwise = 
         map (x:) (combinations (n-1) xs) ++ combinations n xs

-- Problem 27: Group the elements of a set into disjoint subsets.

combination :: Int -> [a] -> [([a],[a])]
combination 0 xs     = [([],xs)]
combination n []     = []
combination n (x:xs) = ts ++ ds
  where ts = [ (x:ys,zs) | (ys,zs) <- combination (n-1) xs ]
        ds = [ (ys,x:zs) | (ys,zs) <- combination  n    xs ]

group :: [Int] -> [a] -> [[[a]]]
group [] = const [[]]
group (n:ns) = concatMap (uncurry $ (. group ns) . map . (:)) . combination n

-- Problem 28: Sorting a list of lists according to length of sublists.

lsort :: [[a]] -> [[a]]
lsort = sortBy (\ a b -> P.length a `compare` P.length b)

lfsort :: (Eq a) => [[a]] -> [[a]]
lfsort xs =
    let lm = map length xs
        count c xs = P.length $ filter (==c) xs
        fcount xs ys = count (length xs) lm
    in  sortBy (\ a b -> (fcount a xs) `compare` (fcount b xs)) xs

