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
    emptyHeap = undefined

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
fromList (x:y:rest) = mergeAll ((N 1 x E E) `merge` (N 1 y E E) : fromList rest)
fromList (x:[])     = [N 1 x E E]

mergePairs :: (a -> a -> a) -> [a] -> a
mergePairs _ [] = error "no pairs"
mergePairs _ (x:[]) = x
mergePairs f (x:y:[]) = x `f` y
mergePairs f (x:y:rest) = mergePairs f (x `f` y : [mergePairs f rest])