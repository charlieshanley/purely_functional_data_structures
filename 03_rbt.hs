{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-------------------------------------------------------------------------------
-- 3.3 - Red Black Trees

data Color = Red | Black deriving (Read, Show, Eq)

data RedBlackTree a = E | T Color (RedBlackTree a) a (RedBlackTree a) deriving (Read, Show)

balance :: (Ord a) => RedBlackTree a -> RedBlackTree a
balance (T Black (T Red (T Red a x b) y c) z d) = T Red (T Black a x b) y (T Black c z d)
balance (T Black (T Red a x (T Red b y c)) z d) = T Red (T Black a x b) y (T Black c z d)
balance (T Black a x (T Red (T Red b y c) z d)) = T Red (T Black a x b) y (T Black c z d)
balance (T Black a x (T Red b y (T Red c z d))) = T Red (T Black a x b) y (T Black c z d)
balance alreadyBalanced = alreadyBalanced

insertRB :: (Ord a) => a -> RedBlackTree a -> RedBlackTree a
insertRB x E = T Red E x E
insertRB x t@(T color a y b)
    | x < y     = balance $ T color (insertRB x a) y b
    | x > y     = balance $ T color a y (insertRB x b)
    | otherwise = t

blackRoot :: RedBlackTree a -> RedBlackTree a
blackRoot tree = case tree of
    E           -> E
    (T _ a x b) -> T Black a x b


class Set s a where
    emptySet :: s a
    insert   :: a -> s a -> s a
    isMember :: a -> s a -> Bool


instance (Ord a) => Set RedBlackTree a where
    emptySet = E
    insert x = blackRoot . insertRB x 
    
    isMember _ E = False
    isMember x (T _ a y b)
        | x < y     = isMember x a
        | x > y     = isMember x b
        | otherwise = True

instance Functor RedBlackTree where
    fmap f E = E
    fmap f (T color a x b) = T color (fmap f a) (f x) (fmap f b)