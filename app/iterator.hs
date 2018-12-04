module Iterator where

class Iterator i where
  iterMap :: (a -> b) -> i a -> i b
  iterFold :: (a -> b -> a) -> a -> i b -> a

data List a = Cons a (List a)
            | Nil deriving (Eq, Show)

data Tree a = Branch a (List (Tree a))
            | Leaf deriving (Eq, Show)

data Queue a = Queue (List a) (List a) deriving (Show)

instance (Eq a) => Eq (Queue a) where
    (==) (Queue Nil ys) (Queue Nil xs) = ys == xs
    (==) q r = (rebalance q) == (rebalance r)

enqueue :: Queue a -> a -> Queue a
enqueue (Queue inbox outbox) x = Queue (Cons x inbox) outbox

rebalance (Queue Nil ys) = Queue Nil ys
rebalance (Queue (Cons x xs) ys) = rebalance (Queue xs (Cons x ys))

dequeue :: Queue a -> (Maybe a, Queue a)
dequeue (Queue Nil Nil) = (Nothing, Queue Nil Nil)
dequeue (Queue xs (Cons y ys)) = (Just y, Queue xs ys)
dequeue (Queue xs Nil) = dequeue (rebalance (Queue xs Nil))

instance Iterator List where
  -- TODO
instance Iterator Tree where
  -- TODO
instance Iterator Queue where
  -- TODO
