{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-------------------------------------------------------------------------------
-- 2.1 - Linked lists (stacks)

class Stack s where
    empty   :: s a
    isEmpty :: s a -> Bool
    cons    :: a -> s a -> s a
    hd      :: s a -> a
    tl      :: s a -> s a
    cat     :: s a -> s a -> s a
    tails   :: s a -> s (s a)
    update  :: s a -> Int -> a -> s a

-- The native list
instance Stack [] where
    empty          = []
    isEmpty        = null
    cons           = (:)
    hd             = head
    tl             = tail
    cat            = (++)
    tails []       = [[]]
    tails w@(_:xs) = w : tails xs
    update [] _ _  = error "Bad subscript."
    update (x:xs) 0 y = y:xs
    update (x:xs) i y = x : update xs (i-1) y

-- A custom list/stack
data List a = Nil | Cons a (List a) deriving (Eq, Show, Read)

instance Stack List where
    empty = Nil
    isEmpty Nil = True
    isEmpty _   = False
    cons = Cons
    hd Nil = error "Empty list has no head."
    hd (Cons x _) = x
    tl Nil = error "Empty list has no tail."
    tl (Cons _ xs) = xs
    cat Nil ys = ys
    cat (Cons x xs) ys = Cons x (cat xs ys)
    tails Nil = Nil
    tails (Cons x xs) = Cons x xs `Cons` tails xs
    update Nil _ _ = error "Bad subscript."
    update (Cons x xs) 0 y  = Cons y xs
    update (Cons x xs) i y  = x `Cons` update xs (i-1) y

    -------------------------------------------------------------------------------
-- 2.2 - Binary search trees

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show, Read)

class Set s a where
    emptySet :: s a
    insert   :: s a -> a -> s a
    isMember :: s a -> a -> Bool

instance (Ord a) => Set Tree a where
    emptySet = Empty
    insert Empty x = Node x Empty Empty
    insert t@(Node y less more) x
        | x < y = Node y (insert less x) more
        | x > y = Node y less (insert more x)
        | otherwise = Node y less more
    isMember Empty x = False
    isMember (Node y less more) x
        | x < y = isMember less x
        | x > y = isMember more x
        | otherwise = True


complete :: (Ord a) => a -> Int -> Tree a
complete _ 0 = Empty
complete x d = Node x child child
    where child = complete x (d-1)

-- data KV k v = KV k v

-- instance (Eq k) => Eq (KV k v) where
--     (KV k1 _) == (KV k2 _) = k1 == k2

-- instance (Ord k, Eq k) => Ord (KV k v) where
--     (KV k1 _) `compare` (KV k2 _) = k1 `compare` k2


-- class FiniteMap m x where
--     emptyMap :: m (KV k v)
--     bind     :: k -> v -> m (KV k v) -> m (KV k v)
--     lookup   :: k -> m (KV k v) -> v

-- instance (Ord k, Eq k) => FiniteMap 