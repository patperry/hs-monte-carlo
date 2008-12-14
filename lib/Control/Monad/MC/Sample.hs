{-# LANGUAGE TypeOperators #-}
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
    sampleIntWithProbs,
    sampleIntSubset,
    sampleIntSubsetWithWeights,
    ) where

import Control.Monad
import Control.Monad.ST
import Control.Monad.MC.Base

import Data.Array.Vector

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

-- | @sampleIntWithProbs ps n@ samples integers from @[ 0..n-1 ]@ with the
-- probability of choosing @i@ equal to @ps !! i@.  The list @ps@ must
-- have length equal to @n@ and sum to @1@.
sampleIntWithProbs :: (MonadMC m) => [Double] -> Int -> m Int
sampleIntWithProbs ps n =
    let qjs = computeTable ps n
    in liftM (indexTable qjs) (uniform 0 1)
{-# INLINE sampleIntWithProbs #-}

-- | Compute the table for use in Walker's aliasing method.  See section
-- III.4 of Luc Devroye's book "Non-Uniform Random Variate Generation",
-- which is available on his homepage, for a description of how it works.
computeTable :: [Double] -> Int -> UArr (Double :*: Int)
computeTable ps n = runST $ do
    sets <- newMU n :: ST s (MUArr Int s)
    qjs  <- newMU n :: ST s (MUArr (Double :*: Int) s)

    nsmall <- partitionProbs sets qjs
    breakLarger sets nsmall qjs
    scaleTable qjs

    unsafeFreezeAllMU qjs

  where
    toDouble = (realToFrac . toInteger) :: Int -> Double
    nd = toDouble n
    uninitialized = -1

    -- Store @map (n*) ps@ in the @qs@ array, and partition the probabilities
    -- into two sets, 'Greater', and 'Smaller' based on whether or not
    -- @q >= 1@ or @q < 1@.  Store the indices of the two sets in the @sets@
    -- array and return @nsmall@, the number of elements in 'Smaller'.  The
    -- sets are stored such that for @i in { 0, ..., nsmall-1 }@,
    -- @qs[sets[i]] <= 1@, and for @i >= nsmall@, @qs[sets[i]] < 1@.
    partitionProbs sets qjs = liftM snd $
        foldM (\(smaller,greater) (i,p) -> do
                   let q = nd*p
                   writeMU qjs i (q :*: uninitialized)
                   if q < 1
                       then do
                           writeMU sets smaller i
                           return (smaller+1,greater)
                       else do
                           writeMU sets greater i
                           return (smaller,greater-1) )
              (0,n-1)
              (zip [0 .. n-1] ps)

    -- Intialize the aliasing table qjs.  The lth entry of the table represents
    -- a mixture distribution with weight q[l] on l and weight (1-q[l]) on
    -- j[l].  qjs[l] stores the pair q[l] :*: j[l].
    breakLarger sets nsmall qjs = let
        breakLargerHelp nsmall' i | nsmall' == 0 = return ()
                                  | otherwise    = do
            -- while Smaller is not empty
            -- choose k from Greater, l from Smaller
            k  <- readMU sets $ nsmall'
            l  <- readMU sets $ i
            qk <- liftM fstS $ readMU qjs k
            ql <- liftM fstS $ readMU qjs l

            -- set jl := k, finalize (ql,jl)
            let jl = k
            writeMU qjs l (ql :*: jl)

            -- set qk := qk - (1-ql)
            let qk' = qk - (1-ql)
            writeMU qjs k (qk' :*: uninitialized)

            -- if qk' < 1, move k from Greater to Smaller
            let nsmall'' = if qk' < 1 then nsmall' else nsmall'-1

            breakLargerHelp nsmall'' (i+1)
        in
            breakLargerHelp nsmall 0

    -- Scale the probabilities in the table so that the lth entry
    -- stores q[l] + l instead of q[l].  This helps when we are sampling
    -- from the table.
    scaleTable qjs =
        forM_ [ 0..(n-1) ] $ \l -> do
            (ql :*: jl) <- readMU qjs l
            writeMU qjs l ((ql + toDouble l) :*: jl)

-- | Given an alias table and a number in the range [0,1),
-- get the corresponding sample in the table.
indexTable :: UArr (Double :*: Int) -> Double -> Int
indexTable qjs u = let
    n  = lengthU qjs
    nu = u * (realToFrac . toInteger) n
    l  = floor nu
    (ql :*: jl) = indexU qjs l
    in if nu < ql then l else jl


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
