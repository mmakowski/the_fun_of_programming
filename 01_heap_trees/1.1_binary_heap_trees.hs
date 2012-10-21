module BinaryHeapTrees where

data Tree a = Null | Fork a (Tree a) (Tree a)
  deriving Show

-- priority queues

isEmpty :: Tree a -> Bool
isEmpty Null = True
isEmpty _    = False

minElem :: Tree a -> a
minElem (Fork e _ _) = e

deleteMin :: Ord a => Tree a -> Tree a
deleteMin (Fork e l r) = merge l r

insert :: Ord a => Tree a -> a -> Tree a
insert t e = merge t (Fork e Null Null)

-- merge

merge :: Ord a => Tree a -> Tree a -> Tree a
merge Null t = t
merge t Null = t
merge t1 t2
  | minElem t1 <= minElem t2 = join t1 t2
  | otherwise                = join t2 t1

-- there is at least 6 straightforward ways in which we can implement join:

join1 (Fork e l r) t = Fork e l (merge r t)
join2 (Fork e l r) t = Fork e r (merge l t)
join3 (Fork e l r) t = Fork e t (merge l r)
join4 (Fork e l r) t = Fork e (merge r t) l
join5 (Fork e l r) t = Fork e (merge l t) r
join6 (Fork e l r) t = Fork e (merge l r) t

{- 
Exercise 1.1

* `join1` produces rightist linear tree and
* `join5` produces leftist linear tree.

E.g. `join1` can be proven by assuming there is going to be a left branch in a tree that results
from a series of joins. If so, there must have been a join in the series that first
introduced a left branch; but a left branch is always constructed from the left branch
of the first input tree, so it must have existed before -- contradiction.
-}

-- let's pick strategy 2) for now:
join :: Ord a => Tree a -> Tree a -> Tree a
join = join2

