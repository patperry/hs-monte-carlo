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
    ) where

import Control.Monad
import Control.Monad.MC.Base
import Control.Monad.MC.Walker

import Data.Array.Vector
import GHC.Arr

-- | @sample n xs@ samples a value uniformly from @take n xs@.  The results
-- are undefined if @length xs@ is less than @n@.
sample :: (MonadMC m) => Int -> [a] -> m a
sample n xs = 
    sampleHelp n xs $ sampleInt n
{-# INLINE sample #-}

-- | @sampleWithWeights ws n xs@ samples a value from @take n xs@, putting
-- weight @ws !! i@ on element @xs !! i@.  The results
-- are undefined if @length xs@ or @length ws@ is less than @n@.
sampleWithWeights :: (MonadMC m) => [Double] -> Int -> [a] -> m a
sampleWithWeights ws n xs = 
    sampleHelp n xs $ sampleIntWithWeights ws n
{-# INLINE sampleWithWeights #-}

-- | @sampleSubset n k xs@ samples a subset of size @k@ from @take n xs@.  The
-- subset elements will appear in the same order that they appear in @xs@.  The
-- results are undefined if @k > n@ or if @length xs < n@.
sampleSubset :: (MonadMC m) => Int -> Int -> [a] -> m [a]
sampleSubset n k xs =
    sampleListHelp n xs $ sampleIntSubset n k
{-# INLINE sampleSubset #-}

sampleHelp :: (Monad m) => Int -> [a] -> m Int -> m a
sampleHelp n xs f = let
    arr = listArray (0,n-1) xs
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
sampleListHelp n xs f = let
    arr = listArray (0,n-1) xs
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

-- | @sampleIntSubset n k@ samples a subset of size @k@ from the 
-- integers @{ 0, ..., n-1 }@.  The return value is a list of length @k@
-- with the elements in the subset.  The elements will be in sorted order.
-- Note also that the elements are lazily generated.
sampleIntSubset :: (MonadMC m) => Int -> Int -> m [Int]
sampleIntSubset n k | k < 0     = fail "negative subset size"
                    | k > n     = fail "subset size is too big"
                    | otherwise = sampleIntSubsetHelp 0 k
  where
    sampleIntSubsetHelp i k' | k' == 0   = return []
                             | otherwise = unsafeInterleaveMC $ do
        u <- uniform 0 1
        if fromIntegral (n-i) * u < fromIntegral k'
            then liftM (i:) $ sampleIntSubsetHelp (i+1) (k'-1)
            else              sampleIntSubsetHelp (i+1)  k'
{-# INLINE sampleIntSubset #-}
