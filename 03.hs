{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-------------------------------------------------------------------------------
-- 3.1 - Leftist heaps

class (Ord a) => Heap h a where
    emptyHeap :: h a
    isEmpty   :: h a ->  Bool
    insert    :: a -> h a -> h a
    merge     :: h a -> h a -> h a
    findMin   :: h a -> a
    deleteMin :: h a -> h a


data H a = E | N  Int a (H a) (H a) deriving (Read, Show)

instance (Ord a) => Heap H a where
    emptyHeap = E

    isEmpty E = True
    isEmpty _ = False

    insert x = merge (N 1 x E E)

    merge h E = h
    merge E h = h
    merge h1@(N _ x a1 b1) h2@(N _ y a2 b2)
        | x <= y    = trans (N 0 x a1 (merge b1 h2))
        | otherwise = trans (N 0 y a2 (merge h1 b2))

    findMin (N _ x _ _) = x
    findMin E = error "No minimum of empty heap."

    deleteMin (N _ _ a b) = merge a b
    deleteMin E = error "No min of empty heap."

rank :: H a -> Int
rank E = 0
rank (N r _ _ _) = r

trans :: H a -> H a
trans (N _ x a b)
    | rank a >= rank b = N (rank b + 1) x a b
    | otherwise        = N (rank a + 1) x b a



fromList :: (Ord a) => [a] -> H a
fromList []         = E
fromList xs         = foldAll merge singletons
    where singletons = flip map xs $ \x -> N 1 x E E

foldPairs :: (a -> a -> a) -> [a] -> [a]
foldPairs _ [] = error "no pairs"
foldPairs _ (x:[]) = [x]
foldPairs f (x:y:[]) = [f x y]
foldPairs f (x:y:rest) = f x y : foldPairs f rest

foldAll :: (a -> a -> a) -> [a] -> a
foldAll _ []       = error "Sorry folks; 'taint a total funct. No empty lists."
foldAll _ (x:[])   = x
foldAll f (x:y:[]) = f x y
foldAll f xs       = foldAll f $ foldPairs f xs

-------------------------------------------------------------------------------
-- 3.2 - Binomial heaps

data Rose a = Rose Int a [Rose a] deriving (Eq, Show, Read)

newtype BinomialHeap a = BH [Rose a]

instance (Ord a) => Heap BinomialHeap a where
    emptyHeap = undefined
    isEmpty = undefined

    insert x bh = 
    
    merge = undefined
    findMin = undefined
    deleteMin = undefined

rank' :: Rose a -> Int
rank' (Rose i _ _) = i
