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
    sampleSubsetWithWeights,

    -- * Sampling @Int@s
    sampleInt,
    sampleIntWithWeights,
    sampleIntSubset,
    sampleIntSubsetWithWeights,
    ) where

import Control.Monad.MC.Base

sample :: (MonadMC m) => n -> [a] -> m a
sample = undefined
{-# INLINE sample #-}

-- | @sampleInt n@ samples integers uniformly from @[ 0..n-1 ]@.  It is an
-- error to call this function with a non-positive @n@.
sampleInt :: (MonadMC m) => Int -> m Int
sampleInt n | n < 1     = fail "invalid argument"
            | otherwise = uniformInt n
{-# INLINE sampleInt #-}

sampleWithWeights :: (MonadMC m) => [Double] -> Int -> [a] -> m a
sampleWithWeights = undefined
{-# INLINE sampleWithWeights #-}

-- | @sampleIntWithWeights ws n@ samples integers from @[ 0..n-1 ]@ with the
-- probability of choosing @i@ proportional to @ws !! i@.  The list @ws@ must
-- have length equal to @n@.  Also, the elements of @ws@ must be non-negative
-- with at least one nonzero entry.
sampleIntWithWeights :: (MonadMC m) => [Double] -> Int -> m Int
sampleIntWithWeights = undefined
{-# INLINE sampleIntWithWeights #-}

sampleSubset :: (MonadMC m) => Int -> Int -> [a] -> m [a]
sampleSubset = undefined
{-# INLINE sampleSubset #-}

sampleIntSubset :: (MonadMC m) => Int -> Int -> m [Int]
sampleIntSubset = undefined
{-# INLINE sampleIntSubset #-}

sampleSubsetWithWeights :: (MonadMC m) => [Double] -> Int -> Int -> [a] -> m [a]
sampleSubsetWithWeights = undefined
{-# INLINE sampleSubsetWithWeights #-}

sampleIntSubsetWithWeights :: (MonadMC m) => [Double] -> Int -> Int -> m [Int]
sampleIntSubsetWithWeights = undefined
{-# INLINE sampleIntSubsetWithWeights #-}
