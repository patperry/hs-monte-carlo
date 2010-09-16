-----------------------------------------------------------------------------
-- |
-- Module     : Data.Summary.Double
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
-- Stability  : experimental
--
-- Summary statistics for @Double@s.
--

module Data.Summary.Double (
    -- * The @Summary@ data type
    Summary,
    summary,
    update,
    
    -- * @Summary@ properties
    sampleSize,
    sampleMin,
    sampleMax,
    sampleMean,
    sampleSE,
    sampleVar,
    sampleSD,
    sampleCI,

    ) where

import Data.List( foldl' )
import Text.Printf

import Data.Summary.Utils


-- | A type for storing summary statistics for a data set including
-- sample size, min and max values, and first and second moments.
data Summary = S {-# UNPACK #-} !Int     -- sample size
                 {-# UNPACK #-} !Double  -- sample mean
                 {-# UNPACK #-} !Double  -- sum of squares
                 {-# UNPACK #-} !Double  -- sample min
                 {-# UNPACK #-} !Double  -- sample max

instance Show Summary where
    show s@(S n mu _ l h) = 
        printf "    sample size: %d" n
        ++ printf "\n            min: %g" l
        ++ printf "\n            max: %g" h
        ++ printf "\n           mean: %g" mu
        ++ printf "\n             SE: %g" (sampleSE s)
        ++ printf "\n         99%% CI: (%g, %g)" c1 c2
      where (c1,c2) = sampleCI 0.99 s

-- | Get a summary of a list of values.
summary :: [Double] -> Summary
summary = foldl' update empty
    
-- | Get an empty summary.
empty :: Summary
empty = S 0 0 0 (1/0) (-1/0)

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

-- | Get a Central Limit Theorem-based confidence interval for the mean
-- with the specified coverage level.  The level must be in the range @(0,1)@.
sampleCI :: Double -> Summary -> (Double,Double)
sampleCI level s = interval level (sampleMean s) (sampleSE s)

-- | Get the minimum of the sample.
sampleMin :: Summary -> Double
sampleMin (S _ _ _ l _) = l

-- | Get the maximum of the sample.
sampleMax :: Summary -> Double
sampleMax (S _ _ _ _ h) = h
