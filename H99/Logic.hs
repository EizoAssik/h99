module H99.Logic where

import Control.Applicative
import Control.Arrow
import Data.Bits (testBit, complementBit)
import H99.Flow
import Numeric
import Data.Char
import Data.List

-- Problem 46

and' True True = True
and' _    _    = False

or' True _ = True
or' _ True = True
or' _ _ = False

not' True = False
not' False = True

nand a = not' . and' a

nor a = not' . or' a

xor True False = True
xor False True = True
xor _ _ = False

equ True True = True
equ False False = True
equ _ _ = False
impl a = not' . equ a

table :: (Bool -> Bool -> Bool) -> IO ()
table fn = do
    mapM_ (putStrLn . fmt) $ zip <$> id <*> map (fn <$> fst <*> snd) $ mt
    where bl = [True, False]
          mt = [(a, b) | a <- bl, b <- bl] 
          fmt = unwords . map show . (g<$>fst.fst<*>snd.fst<*>snd)
          g a b c = [a, b, c]

-- Problem 47

-- Solution 46 just works.

-- Problem 48

tablen :: Int -> ([Bool] -> Bool) -> IO ()
tablen n fn = do
    mapM_ (putStrLn . fmt) $ zip <$> id <*> map fn $ mt
    where mt = map (\ i -> map (testBit i) [n-1,n-2..0]) ([2^n-1,2^n-2..0] :: [Int])
          fmt = unwords . map show . ((++)<$>fst<*>return . snd)

-- Problem 49

gray :: Int -> [Int]
gray 1 = [0, 1]
gray n = ln ++ (map fn $ reverse ln)
    where fn = (flip complementBit) (n-1)
          ln = gray (n-1)

-- Problem 50
type HuffmanPair = (HuffmanTree, HuffmanTree)
data HuffmanTree = HNode Int Char
                 | HTree Int HuffmanPair
                 deriving (Eq, Show)

huffreq (HTree freq _) = freq
huffreq (HNode freq _) = freq

huffmerge a b = HTree (huffreq a + huffreq b) (a, b)

huffman :: [(Char, Int)] -> [(Char, String)]
huffman [] = []
huffman xs = 
    let ts = map p2ht $ sortBy pcmp xs
    in  huffdump [] $ huffman' ts
    where
      p2ht = HNode <$> snd <*> fst
      pcmp a b = snd a `compare` snd b
      hcmp a b = huffreq a `compare` huffreq b
      huffdump xs (HNode f c) = [(c, reverse xs)]
      huffdump xs (HTree f (a, b)) = huffdump ('0':xs) a ++ huffdump ('1':xs) b
      huffman' (x:[]) = x
      huffman' (x:y:zs) = huffman' $ sortBy hcmp $ huffmerge x y:zs

    


