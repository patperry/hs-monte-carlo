-----------------------------------------------------------------------------
-- |
-- Module     : Control.Monad.MC.Summary
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

module Control.Monad.MC.Summary (
    -- * Summary statistics
    -- ** The @Summary@ data type
    Summary,
    summary,
    update,
    
    -- ** @Summary@ properties
    sampleSize,
    sampleMean,
    sampleVar,
    sampleSD,
    sampleSE,
    sampleMin,
    sampleMax,
    
    ) where

-- | A type for storing summary statistics for a data set including
-- sample size, min and max values, and first and second moments.
data Summary = S {-# UNPACK #-} !Int     -- sample size
                 {-# UNPACK #-} !Double  -- sample mean
                 {-# UNPACK #-} !Double  -- sum of squares
                 {-# UNPACK #-} !Double  -- sample min
                 {-# UNPACK #-} !Double  -- sample max
    
-- | Get an empty summary.
summary :: Summary
summary = S 0 0 0 (1/0) (-1/0)

-- | Update the summary with a data point.  
-- Running mean and variance computed as in Knuth, Vol 2, page 232, 
-- 3rd edition, see http://www.johndcook.com/standard_deviation.html for
-- a description.
update :: Summary -> Double -> Summary
update (S n m s l h) x =
    let n'    = n+1
        delta = x - m
        m'    = m + delta / fromIntegral n'
        s'    = s + delta*(x - m')
        l'    = if x < l then x else l
        h'    = if x > h then x else h
    in S n' m' s' l' h'

-- | Get the sample size.
sampleSize :: Summary -> Int
sampleSize (S n _ _ _ _) = n

-- | Get the sample mean.
sampleMean :: Summary -> Double
sampleMean (S _ m _ _ _) = m

-- | Get the sample variance.
sampleVar :: Summary -> Double
sampleVar (S n _ s _ _) = s / fromIntegral (n - 1)

-- | Get the sample standard deviation.
sampleSD :: Summary -> Double
sampleSD s = sqrt (sampleVar s)

-- | Get the sample standard error.
sampleSE :: Summary -> Double
sampleSE s = sqrt (sampleVar s / fromIntegral (sampleSize s))

-- | Get the minimum of the sample.
sampleMin :: Summary -> Double
sampleMin (S _ _ _ l _) = l

-- | Get the maximum of the sample.
sampleMax :: Summary -> Double
sampleMax (S _ _ _ _ h) = h
