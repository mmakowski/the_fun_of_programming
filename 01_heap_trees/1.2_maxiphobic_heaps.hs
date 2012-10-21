module MaxiphobicHeaps where

data Tree a = Null | Fork Int a (Tree a) (Tree a)
  deriving Show

isEmpty :: Tree a -> Bool
isEmpty Null = True
isEmpty _    = False

minElem :: Tree a -> a
minElem (Fork _ e _ _) = e

deleteMin :: Ord a => Tree a -> Tree a
deleteMin (Fork _ e l r) = merge l r

insert :: Ord a => a -> Tree a -> Tree a
insert e t = merge (Fork 1 e Null Null) t

merge :: Ord a => Tree a -> Tree a -> Tree a
merge Null t = t
merge t Null = t
merge t1 t2
  | minElem t1 <= minElem t2 = join t1 t2
  | otherwise                = join t2 t1

join :: Ord a => Tree a -> Tree a -> Tree a
join (Fork n e l r) t = Fork (n + size t) e a (merge b c)
  where (a, b, c) = orderBySize l r t

orderBySize :: Tree a -> Tree a -> Tree a -> (Tree a, Tree a, Tree a)
orderBySize a b c
  | size a == biggest = (a, b, c)
  | size b == biggest = (b, a, c)
  | size c == biggest = (c, a, b)
  where biggest = size a `max` size b `max` size c

size :: Tree a -> Int
size Null           = 0
size (Fork n _ _ _) = n

{-
## Exercise 1.2

1) 
            1
           /
          2
         / 
        3
       / 
      4
     /
    5
   /
  6
 /
7

2)
      1
    _/ \_
   3     2
  / \   / \
 7   5 6   4

3)

      2
    _/ \_
   3     5
  / \   /
 4   7 6   

-}

{- 
## Exercise 1.3

for linear heap:

* `deleteMin` runs in constant time
* `insert` runs in constant time

`log n` recursive invocations of `join` when merging is the worst-case scenario. For unblalanced
heaps the number of recursive invocations is smaller than this.
-}
