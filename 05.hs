
-- A Queue is a thing you add to the end of, and take from the front of.
-- head (taking) and snoc (adding) should be cheap.

class Queue q where
    emptyQ   :: q a
    isEmptyQ :: q a -> Bool
    snocQ    :: q a -> a -> q a
    headQ    :: q a -> a
    tailQ    :: q a -> q a

data Q a = Q [a] [a] deriving (Eq, Read, Show)

instance Queue Q where
    emptyQ = Q [] []
    isEmptyQ (Q [] []) = True
    isEmptyQ (Q _  _)  = False
    snocQ (Q back front) x = checkQ $ Q (x:back) front
    headQ (Q _ (x:xs))     = x
    tailQ (Q back (x:xs))  = checkQ $ Q back xs

checkQ :: Q a -> Q a
checkQ (Q back []) = Q [] (reverse back)
checkQ q              = q

-- As you can see, snoc and headQ run in O(1) worst-case time.
-- The expensive operation is the reversing of the `back` list when we run out
-- of elements in `front` and need to migrate the contents of `back`.
-- This (the reversing operation) runs in O(n) time, where n is the length of
-- `back`. Thus, the worst-case runtime of snoc and tailQ is O(n)

-- However, using the banker's method or the physicist's method, we can see
-- that, amortized, snoc and tailQ both take 0(1) time. In essense, each time we
-- snoc an element to the end of the list, we pay for one step, and commit
-- ourselves to pay one additional step in the future, as part of the eventual
-- reversing of `back`, for an amortized time of O(2). Then when we must reverse
-- `back`, we have already accounted for the cost.



-- A double-ended queue, or dequeue, is a thing you can add and take from the
-- front or back of. Adding and taking from either end should be cheap.

-- The implementaion complication with this one is that, when one end is empty,
-- we need to migrate only half to the otherside. How can we take half?
-- Asking for the length is too expensive. We need to hold the length as data
-- and do the accounting as we manipulate it. Then we can just look up the
-- length when we need it.

-- But wait, is the amortized cost of just counting the length once equivalent
-- to the amortized cost of holding it as data and accounting for each change?
-- Will think about that.

class Dequeue q where
    emptyD   :: q a
    isEmptyD :: q a -> Bool
    -- insert, inspect, remove front element
    consD :: q a -> a -> q a
    headD :: q a -> a
    tailD :: q a -> q a
    -- insert, inspect, remove back element
    snocD  :: q a -> a -> q a
    lastD  :: q a -> a
    initD  :: q a -> q a

instance Dequeue Q where
    emptyD (Q [] []) = True
    emptyD _         = False

    consD (Q back front) x = Q back (x:front)
    headD (Q _ (x:xs))     = x
    tailD (Q back (x:xs))  = Q back xs

    snocD (Q back front) x = Q (x:back) front
    lastD (Q (x:xs) front) = x
    initD (Q (x:xs) front) = Q xs front

checkD :: Q a -> Q a
checkD (Q [] front) = 
checkD (Q back [])  =
checkD d            = d