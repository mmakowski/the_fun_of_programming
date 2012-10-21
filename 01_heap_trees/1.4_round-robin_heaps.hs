module RoundRobinHeaps where

data Tree a = Null | Fork a (Tree a) (Tree a)
  deriving Show

isEmpty :: Tree a -> Bool
isEmpty Null = True
isEmpty _    = False

minElem :: Tree a -> a
minElem (Fork e _ _) = e

deleteMin :: Ord a => Tree a -> Tree a
deleteMin (Fork e l r) = merge l r

insert :: Ord a => a -> Tree a -> Tree a
insert e t = merge (Fork e Null Null) t

merge :: Ord a => Tree a -> Tree a -> Tree a
merge Null t = t
merge t Null = t
merge t1 t2
  | minElem t1 <= minElem t2 = join t1 t2
  | otherwise                = join t2 t1

-- skew heap: we always insert to the left subtree, but after doing so replace left with right
-- so that next time we'll instert to the other sub-tree; this is equivalent to a 
-- round-robin heap

join :: Ord a => Tree a -> Tree a -> Tree a
join (Fork e l r) t = Fork e r (merge l t)

{-
## Exercise 1.5

1) 

(insert 2 . insert 1 . insert 4 . insert 3 . insert 6 . insert 5 . insert 8 . insert 7 . insert 9) Null

will look the same when skew heap is used.

2)

can't produce it from atomic nodes?
-}