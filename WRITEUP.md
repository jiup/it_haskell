# Iterator in Haskell


#### File Structure
```
hw07.zip
├── README.pdf
├── iterator.hs
└── test.hs
```



#### Implementation

```haskell
instance Iterator List where
  iterMap _ Nil = Nil
  iterMap f (x `Cons` xs) = f x `Cons` iterMap f xs
  iterFold _ v Nil = v
  iterFold f v (x `Cons` xs) = iterFold f (f v x) xs

instance Iterator Tree where
  iterMap _ Leaf = Leaf
  -- The first `iterMap` on the right is to execute map on list `xs`,
  -- and the second `iterMap` is for recursively update a Tree in `xs`
  iterMap f (Branch x xs) = f x `Branch` iterMap (iterMap f) xs
  iterFold _ v Leaf = v
  iterFold f v (Branch x xs) = iterFold (iterFold f) (f v x) xs

instance Iterator Queue where
  iterMap _ (Queue Nil Nil) = Queue Nil Nil
  iterMap f (Queue xs ys) = (Queue (iterMap f xs) (iterMap f ys))
  iterFold _ v (Queue Nil Nil) = v
  -- To assure the order, I rebalance the queue first, and then
  -- fold the list `ys` to get the collected value `v`
  iterFold f v (Queue Nil ys) = iterFold f v ys
  iterFold f v (Queue xs ys) = iterFold f v (rebalance (Queue xs ys))
  
```



#### Run Test Code

**Input:**

```shell
$ runhaskell test.hs
```

**Output:**
```shell
PASS
...

[32/32]%
```