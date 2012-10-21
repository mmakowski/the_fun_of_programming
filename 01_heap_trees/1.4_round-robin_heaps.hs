module RoundRobinHeaps where

data Colour = Blue | Red
  deriving Show

data Tree a = Null | Fork Colour a (Tree a) (Tree a)
  deriving Show

isEmpty :: Tree a -> Bool
isEmpty Null = True
isEmpty _    = False

minElem :: Tree a -> a
minElem (Fork _ e _ _) = e

deleteMin :: Ord a => Tree a -> Tree a
deleteMin (Fork _ e l r) = merge l r

insert :: Ord a => a -> Tree a -> Tree a
insert e t = merge (Fork Blue e Null Null) t

merge :: Ord a => Tree a -> Tree a -> Tree a
merge Null t = t
merge t Null = t
merge t1 t2
  | minElem t1 <= minElem t2 = join t1 t2
  | otherwise                = join t2 t1

join :: Ord a => Tree a -> Tree a -> Tree a
join (Fork Blue e l r) t = Fork Red e (merge l t) r
join (Fork Red e l r) t = Fork Blue e l (merge r t)
