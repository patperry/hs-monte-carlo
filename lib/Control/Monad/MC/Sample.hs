{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -XPatternSignatures -fno-warn-deprecated-flags#-}
-----------------------------------------------------------------------------
-- |
-- Module     : Control.Monad.MC.Sample
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

module Control.Monad.MC.Sample (
    -- * Sampling from lists
    sample,
    sampleWithWeights,
    sampleSubset,

    -- * Sampling @Int@s
    sampleInt,
    sampleIntWithWeights,
    sampleIntSubset,
    
    -- * Shuffling
    shuffle,
    shuffleInt,
    ) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.MC.Base
import Control.Monad.MC.Walker

import Data.Array.Base
import Data.Array.IArray
import Data.Array.ST
import Data.Array.Vector

-- | @sample xs@ samples a value uniformly from the elements of @xs@.  The
-- results are undefined if @length xs@ is zero.
sample :: (MonadMC m) => [a] -> m a
sample xs = let
    n = length xs
    in sampleHelp n xs $ sampleInt n
{-# INLINE sample #-}

-- | @sampleWithWeights ws n xs@ samples a value from @xs@, putting
-- weight @ws !! i@ on element @xs !! i@.
sampleWithWeights :: (MonadMC m) => [Double] -> [a] -> m a
sampleWithWeights ws xs = let
    n = length ws
    in sampleHelp n xs $ sampleIntWithWeights ws n
{-# INLINE sampleWithWeights #-}

-- | @sampleSubset k xs@ samples a subset of size @k@ from @xs@ by 
-- sampling without replacement.  The return value is a list of length @k@ 
-- with the elements in the subset in the order that they were sampled.  Note
-- also that the elements are lazily generated.
sampleSubset :: (MonadMC m) => Int -> [a] -> m [a]
sampleSubset k xs = let
    n = length xs
    in sampleListHelp n xs $ sampleIntSubset k n
{-# INLINE sampleSubset #-}

sampleHelp :: (Monad m) => Int -> [a] -> m Int -> m a
sampleHelp n (xs :: [a]) f = let
    arr = listArray (0,n-1) xs :: Array Int a
    in liftM (unsafeAt arr) f

sampleHelpUA :: (UA a, Monad m) => Int -> [a] -> m Int -> m a
sampleHelpUA n xs f = let
    arr = newU n (\marr -> zipWithM_ (writeMU marr) [0..n-1] xs)
    in liftM (indexU arr) f

{-# RULES "sampleHelp/Double" forall n xs f.
              sampleHelp n (xs :: [Double]) f = sampleHelpUA n xs f #-}
{-# RULES "sampleHelp/Int" forall n xs f.
              sampleHelp n (xs :: [Int]) f = sampleHelpUA n xs f #-}

sampleListHelp :: (Monad m) => Int -> [a] -> m [Int] -> m [a]
sampleListHelp n (xs :: [a]) f = let
    arr = listArray (0,n-1) xs :: Array Int a
    in liftM (map $ unsafeAt arr) f

sampleListHelpUA :: (UA a, Monad m) => Int -> [a] -> m [Int] -> m [a]
sampleListHelpUA n xs f = let
    arr = newU n (\marr -> zipWithM_ (writeMU marr) [0..n-1] xs)
    in liftM (map $ indexU arr) f

{-# RULES "sampleListHelp/Double" forall n xs f.
              sampleListHelp n (xs :: [Double]) f = sampleListHelpUA n xs f #-}
{-# RULES "sampleListHelp/Int" forall n xs f.
              sampleListHelp n (xs :: [Int]) f = sampleListHelpUA n xs f #-}

-- | @sampleInt n@ samples integers uniformly from @[ 0..n-1 ]@.  It is an
-- error to call this function with a non-positive @n@.
sampleInt :: (MonadMC m) => Int -> m Int
sampleInt n | n < 1     = fail "invalid argument"
            | otherwise = uniformInt n
{-# INLINE sampleInt #-}

-- | @sampleIntWithWeights ws n@ samples integers from @[ 0..n-1 ]@ with the
-- probability of choosing @i@ proportional to @ws !! i@.  The list @ws@ must
-- have length equal to @n@.  Also, the elements of @ws@ must be non-negative
-- with at least one nonzero entry.
sampleIntWithWeights :: (MonadMC m) => [Double] -> Int -> m Int
sampleIntWithWeights ws n =
    let qjs = computeTable n ws
    in liftM (indexTable qjs) (uniform 0 1)
{-# INLINE sampleIntWithWeights #-}

-- | @sampleIntSubset k n@ samples a subset of size @k@ by sampling without
-- replacement from the integers @{ 0, ..., n-1 }@.  The return value is a 
-- list of length @k@ with the elements in the subset in the order that they
-- were sampled.  Note also that the elements are lazily generated.
sampleIntSubset :: (MonadMC m) => Int -> Int -> m [Int]
sampleIntSubset k n | k < 0     = fail "negative subset size"
                    | k > n     = fail "subset size is too big"
                    | otherwise = do
    us <- randomIndices k n
    return $ runST $ do
        ints <- newMU n
        sequence_ [ writeMU ints i i | i <- [0 .. n-1] ]
        sampleIntSubsetHelp ints us (n-1)
  where
    randomIndices k' n' | k' == 0   = return []
                        | otherwise = unsafeInterleaveMC $ do
        u  <- uniformInt n'
        us <- randomIndices (k'-1) (n'-1)
        return (u:us)
        
    sampleIntSubsetHelp _    []     _  = return []
    sampleIntSubsetHelp ints (u:us) n' = unsafeInterleaveST $ do
        i <- readMU ints u
        writeMU ints u =<< readMU ints n'
        is <- sampleIntSubsetHelp ints us (n'-1)
        return (i:is)
{-# INLINE sampleIntSubset #-}

-- | @shuffle xs@ randomly permutes the list @xs@ and returns
-- the result.  All permutations of the elements of @xs@ are equally
-- likely.
shuffle :: (MonadMC m) => [a] -> m [a]
shuffle (xs :: [a]) = let
    n = length xs
    in shuffleInt n >>= \swaps -> (return . runST) $ do
           marr <- newListArray (0,n-1) xs :: ST s (STArray s Int a)
           mapM_ (swap marr) swaps
           getElems marr
  where
    swap marr (i,j) | i == j    = return ()
                    | otherwise = do
        x <- unsafeRead marr i
        y <- unsafeRead marr j
        unsafeWrite marr i y
        unsafeWrite marr j x
{-# INLINE shuffle #-}

shuffleUA :: (UA a, MonadMC m) => [a] -> m [a]
shuffleUA (xs :: [a]) = let
    n = length xs
    in shuffleInt n >>= \swaps -> (return . runST) $ do
           marr <- newMU n
           zipWithM_ (writeMU marr) [0 .. n-1] xs
           mapM_ (swap marr) swaps
           arr <- unsafeFreezeAllMU marr
           return $ fromU arr
  where
    swap marr (i,j) | i == j    = return ()
                    | otherwise = do
        x <- readMU marr i
        y <- readMU marr j
        writeMU marr i y
        writeMU marr j x
{-# INLINE shuffleUA #-}        

{-# RULES "shuffle/Double" forall xs.
              shuffle (xs :: [Double]) = shuffleUA xs #-}
{-# RULES "shuffle/Int" forall xs.
              shuffle (xs :: [Int]) = shuffleUA xs #-}


-- | @shuffleInt n@ generates a sequence of swaps equivalent to a
-- uniformly-chosen random permutatation of the integers @{0, ..., n-1}@.  
-- For an input of @n@, there are @n-1@ swaps, which are lazily generated.
shuffleInt :: (MonadMC m) => Int -> m [(Int,Int)]
shuffleInt n =
    let shuffleIntHelp i | i <= 1    = return []
                         | otherwise = unsafeInterleaveMC $ do
            j   <- uniformInt i
            ijs <- shuffleIntHelp (i-1)
            return $ (i-1,j):ijs in
    shuffleIntHelp n
{-# INLINE shuffleInt #-}
