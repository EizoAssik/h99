module H99.Arithmetic where

import Prelude hiding (gcd)
import Data.Array.Unboxed
import Data.List
import Control.Applicative
import H99.Flow

-- Problem 31: Determine whether a given integer number is prime.

isPrime :: Int -> Bool
isPrime 2 = True
isPrime n = n > 2 && (all ((/=0) . mod n) $ 2:[3,5..top])
            where top = ceiling . sqrt . fromIntegral $ n

-- Problem 32: GCD

gcd :: Int -> Int -> Int
gcd a 0 = a
gcd a b = gcd b $ a `mod` b

-- Problem 33: coprime

coprime :: Int -> Int -> Bool
coprime a = (1==) . gcd a

-- Problem 34

totientPhi :: Int -> Int
totientPhi m = length $ filter (coprime m) [1..m]

-- Problem 35

primesSA :: [Int]
primesSA = 2 : oddprimes
  where
    oddprimes = 3 : sieve oddprimes 3 []
    sieve (p:ps) x fs = [i*2 + x | (i,True) <- assocs a]
                        ++ sieve ps (p*p) ((p,0) :
                             [(s, rem (y-q) s) | (s,y) <- fs])
     where
      q = (p*p-x)`div`2
      a :: UArray Int Bool
      a = accumArray (\ b c -> False) True (1,q-1)
                     [(i,()) | (s,y) <- fs, i <- [y+s, y+s+s..q]]

primeFactors :: Int -> [Int]
primeFactors n = fn n primesSA
    where fn 1 _ = []
          fn n l@(x:xs) = if mod n x == 0 then x : fn (n`div`x) l else fn n xs

-- Problem 36

primeFactorsMult = map ((,) <$> head <*> length) . group . primeFactors

-- Problem 37

phi :: [(Int, Int)] -> Int
phi = abs . foldl1 (*) . map ((*)<$>((-)1.fst)<*>((^)<$>fst<*>((-)1.snd)))

-- Problem 38          

-- add 'abs' to Problem 37

-- Problem 39

primesR l r = takeWhile (<=r) . dropWhile (<l) $ primesSA

-- Problem 40

goldbach :: Int -> (Int, Int)
goldbach n = (,)<$>id<*>(n-) $ head $ dropWhile (not . isPrime . (-)n) primesSA

-- Problem 41

goldbachList l r = map goldbach <$> filter even $ [l..r]




