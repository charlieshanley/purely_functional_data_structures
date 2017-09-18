
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

-- A custom list/stack
data List a = Nil | Cons a (List a) deriving (Eq, Show, Read)

instance Stack List where
    empty = Nil
    isEmpty Nil = True
    isEmpty _   = False
    cons = Cons
    hd Nil = error "Empty list has no head."
    hd (Cons x _) = x
    tl Nil = error "Empty list has no tail"
    tl (Cons _ xs) = xs
    cat Nil ys = ys
    cat (Cons x xs) ys = Cons x (cat xs ys)
    tails Nil = Nil
    tails (Cons x xs) = Cons x xs `Cons` tails xs

    -------------------------------------------------------------------------------
-- 2.2 - Binary search trees

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Eq, Show, Read)