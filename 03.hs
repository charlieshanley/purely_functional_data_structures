{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

-------------------------------------------------------------------------------
-- 3.1 - Leftist heaps

class Heap h where
    emptyHeap :: (Ord a) => h a
    isEmpty   :: (Ord a) => h a ->  Bool
    insert    :: (Ord a) => a -> h a -> h a
    merge     :: (Ord a) => h a -> h a -> h a
    findMin   :: (Ord a) => h a -> a
    deleteMin :: (Ord a) => h a -> h a


data H a = E | N  Int a (H a) (H a) deriving (Read, Show)

instance Heap H where
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

instance Heap BinomialHeap where
    emptyHeap = BH []
    isEmpty (BH ts) = null ts

    insert x (BH ts) = BH (insTree (Rose 0 x []) ts)
    
    merge (BH ts1) (BH ts2) = BH (mrg ts1 ts2)

    findMin (BH ts) = root t
        where (t, _) = popMinTree ts

    deleteMin (BH ts) = BH $ mrg (reverse ts1) ts2
        where (Rose _ x ts1, ts2) = popMinTree ts

rank' :: Rose a -> Int
rank' (Rose i _ _) = i

root :: Rose a -> a
root (Rose _ x _ ) = x

link :: (Ord a) => Rose a -> Rose a -> Rose a
link t1@(Rose r x1 c1) t2@(Rose _ x2 c2)
    | x1 <= x2  = Rose (r + 1) x1 (t2:c1)
    | otherwise = Rose (r + 1) x2 (t1:c2)

insTree :: (Ord a) => Rose a -> [Rose a] -> [Rose a]
insTree t [] = [t]
insTree t ts@(t':ts')
    | rank' t < rank' t' = t:ts
    | otherwise        = insTree (link t t') ts'

mrg :: Ord a => [Rose a] -> [Rose a] -> [Rose a]
mrg ts [] = ts
mrg [] ts = ts
mrg ts1@(t1:ts1') ts2@(t2:ts2')
    | rank' t1 < rank' t2 = t1 : mrg ts1' ts2
    | rank' t2 < rank' t1 = t2 : mrg ts2' ts1
    | otherwise         = insTree (link t1 t2) (mrg ts1' ts2')

popMinTree [] = error "empty heap"
popMinTree [t] = (t, [])
popMinTree (t:ts)
    | root t < root t' = (t, ts)
    | otherwise        = (t', t:ts')
    where (t', ts') = popMinTree ts
