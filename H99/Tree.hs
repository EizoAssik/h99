module H99.Tree where

import Control.Applicative

data Tree a = Empty
            | Branch a (Tree a) (Tree a)
            deriving (Show, Eq)

-- Problem 55: Construct completely balanced binary trees.

cbalTree :: Int -> [Tree Char]
cbalTree 0 = [Empty]
cbalTree 1 = [Branch 'x' Empty Empty]
cbalTree n =
    let m = (n - 1) `div` 2
        subl = cbalTree m 
        subr = if odd n then subl else cbalTree (1 + m)
        merge a b = [Branch 'x' l r | l <- a, r <- b]
    in  if odd n then merge subl subr else merge subl subr ++ merge subr subl

-- Problem 56

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r
  where mirror Empty Empty = True
        mirror (Branch _ ll lr) (Branch _ rl rr) =
          mirror lr rl && mirror ll rr
        mirror _ _ = False

-- Problem 57 BST

construct :: (Ord a) => [a] -> Tree a
construct [] = Empty
construct (x:xs) = consbst (Branch x Empty Empty) xs
    where consbst t [] = t
          consbst t (x:xs) = insertbst x t `consbst` xs
          insertbst x Empty = Branch x Empty Empty
          insertbst x t@(Branch v l r) = 
              case compare x v of
                  EQ -> t 
                  LT -> Branch v (insertbst x l) r
                  GT -> Branch v l (insertbst x r)

-- Problem 58

symCbalTrees :: Int -> [Tree Char]
symCbalTrees = filter symmetric . cbalTree
    




