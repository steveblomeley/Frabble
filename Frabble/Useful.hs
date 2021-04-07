module Frabble.Useful where

import System.Random
import Data.List

tryFind :: Eq k => k -> [(k,v)] -> Maybe v
tryFind k kvs = if null vs then Nothing else Just (head vs)
                where 
                    vs = [v | (k',v) <- kvs, k' == k]

tryFindPair :: Eq k => k -> [(k,v)] -> Maybe (k,v)
tryFindPair k kvs = if null kvs' then Nothing else Just (head kvs')
                    where 
                        kvs' = [(k',v) | (k',v) <- kvs, k' == k]

tryFindManyPair :: (Eq k, Eq v) => [k] -> [(k,v)] -> [(k,v)]
tryFindManyPair ks kvs = nub [(k,v) | (k,v) <- kvs, k' <- ks, k' == k]         

find :: Eq k => k -> [(k,v)] -> v
find k kvs = head [v | (k',v) <- kvs, k' == k]

findPair :: Eq k => k -> [(k,v)] -> (k,v)
findPair k kvs = head [(k',v) | (k',v) <- kvs, k' == k]

-- Randomly pick tiles from bag to rack
randomPick1 :: [a] -> IO a
randomPick1 xs = do
    i <- randomRIO (0, (length xs) - 1)
    return (xs !! i)    

randomPick :: Eq a => [a] -> Int -> IO [a]
randomPick _ 0  = return []
randomPick xs n = do
    x   <- randomPick1 xs
    xs' <- randomPick (xs `without1` x) (n-1)
    return (x : xs')

without1 :: Eq a => [a] -> a -> [a]
without1 [] y = []
without1 (x:xs) y 
   | x == y    = xs
   | otherwise = x : (xs `without1` y)

without :: Eq a => [a] -> [a] -> [a]
without xs []     = xs
without xs (y:ys) = (xs `without1` y) `without` ys 