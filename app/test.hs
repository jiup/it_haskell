{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import Iterator
import Control.Monad.State
import qualified System.Random as R
import Text.Printf

type RandM a = State R.StdGen a
type IterF a b = forall i. (Iterator i, Eq (i a), Show (i a), Eq (i b), Show (i b)) => i a -> i b

random :: (R.Random a) => RandM a
random = state R.random

data Test = Test String Bool

instance Show Test where
  show (Test s _) = s

mkTest :: (Eq a, Show a) => a -> a -> Test
mkTest x y = let b = x == y
             in Test (testStr x y b) b

testStr x y b = if b
                then "PASS\n"
                else (show x) ++ " should be the same as " ++ (show y) ++ ".\nFAIL\n"

passedRatio :: [Test] -> (Int, Int)
passedRatio tests =
  let
    total = length tests
    pass = length $ filter passed tests
  in (pass, total) where
    passed (Test _ b) = b

listOf' :: (a -> i a -> i a) -> i a -> Double -> RandM a -> RandM (i a)
listOf' c n p r = do d <- (random :: RandM Double)
                     a <- r
                     as <- if d < p then (listOf' c n p r) else return n
                     return (c a as)
builtinListOf :: RandM a -> Double -> RandM [a]
builtinListOf rand p = listOf' (:) [] p rand
listOf :: RandM a -> Double -> RandM (List a)
listOf rand p = listOf' Cons Nil p rand
randList :: (R.Random a) => RandM (List a)
randList = listOf random 0.7

randTree :: forall a. (R.Random a) => RandM (Tree a)
randTree = do head <- random
              p <- (random :: RandM Double)
              if p < 0.2
                then do children <- listOf randTree 0.9 :: RandM (List (Tree a))
                        return (Branch head children)
                else return Leaf
randQueue :: (R.Random a) => RandM (Queue a)
randQueue = do inbox <- randList
               outbox <- randList
               return (Queue inbox outbox)

idTest :: (Iterator i, Show (i a), Eq (i a)) => (i a -> i a) -> i a -> Test
idTest f x = mkTest x (f x)

equalTest :: (Iterator i, Show (i b), Eq (i b)) =>
             i a
          -> (i a -> i b)
          -> (i a -> i b)
          -> Test
equalTest i f g = mkTest (f i) (g i)

composeTest :: (Iterator i, Show (i c), Eq (i c)) => i a -> (b -> c) -> (a -> b) -> Test
composeTest x f g = equalTest x (iterMap f . iterMap g) (iterMap (f . g))

toList lst = iterFold (\x y -> y:x) [] lst

composeFold f i iter = mkTest (iterFold f i iter) ((iterFold (.) id (iterMap (\b -> (\x -> f x b)) iter)) i)

composeMap :: (Iterator i, Show (i a), Eq (i a), Num a) => i a -> Test
composeMap x = composeTest x (*5) (3-)

mapIdTest :: (Iterator i, Show (i a), Eq (i a)) => i a -> Test
mapIdTest x = idTest (iterMap id) x

foldCompTest :: (Iterator i, Show (i a), Eq (i a), Num a, Eq a, Show a) => i a -> Test
foldCompTest = composeFold (\x y -> x + y) 5

listLen :: (Iterator i, Show (i a), Eq (i a)) => i a -> Test
listLen x =
  let
    lenList = length $ toList x :: Int
    lenIter = iterFold (+) (0 :: Int) (iterMap (const 1) x) :: Int
  in mkTest lenList lenIter

applyFuncs :: (Iterator i, Show (i Integer), Eq (i Integer)) => [i Integer] -> [Test]
applyFuncs = ([mapIdTest, composeMap, listLen, foldCompTest] <*>)

testsState :: RandM [Test]
testsState = do
  intlists   <- fmap (applyFuncs :: ([List Integer] -> [Test])) (builtinListOf randList 0.5 :: RandM [List Integer])
  inttrees   <- fmap (applyFuncs :: ([Tree Integer] -> [Test])) (builtinListOf randTree 0.5 :: RandM [Tree Integer])
  intqueues  <- fmap (applyFuncs :: ([Queue Integer] -> [Test])) (builtinListOf randQueue 0.5 :: RandM [Queue Integer])
  return $ intlists ++ inttrees ++ intqueues

main :: IO ()
main = do
  let tests = evalState testsState (R.mkStdGen 254)
  let (passed, total) = passedRatio tests
  forM_ tests print
  printf "[%d/%d]" passed total
