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

import Control.Monad
import Control.Monad.MC.Base
import Control.Monad.MC.Walker

sample :: (MonadMC m) => n -> [a] -> m a
sample = undefined
{-# INLINE sample #-}

sampleWithWeights :: (MonadMC m) => [Double] -> Int -> [a] -> m a
sampleWithWeights = undefined
{-# INLINE sampleWithWeights #-}

sampleSubset :: (MonadMC m) => Int -> Int -> [a] -> m [a]
sampleSubset = undefined
{-# INLINE sampleSubset #-}

sampleSubsetWithWeights :: (MonadMC m) => [Double] -> Int -> Int -> [a] -> m [a]
sampleSubsetWithWeights = undefined
{-# INLINE sampleSubsetWithWeights #-}

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

sampleIntSubset :: (MonadMC m) => Int -> Int -> m [Int]
sampleIntSubset = undefined
{-# INLINE sampleIntSubset #-}

sampleIntSubsetWithWeights :: (MonadMC m) => [Double] -> Int -> Int -> m [Int]
sampleIntSubsetWithWeights = undefined
{-# INLINE sampleIntSubsetWithWeights #-}
