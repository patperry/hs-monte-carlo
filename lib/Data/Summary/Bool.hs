-----------------------------------------------------------------------------
-- |
-- Module     : Data.Summary.Bool
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
-- Stability  : experimental
--
-- Summary statistics for @Bool@s.
--

module Data.Summary.Bool (
    -- * The @Summary@ data type
    Summary,
    summary,
    update,

    -- * @Summary@ properties
    sampleSize,
    count,
    sampleMean,
    sampleSE,
    sampleCI,

    ) where

import Control.DeepSeq
import Data.List( foldl' )
import Data.Monoid
import Text.Printf

import Data.Summary.Utils


-- | A type for storing summary statistics for a data set of
-- booleans.  Specifically, this just keeps track of the number
-- of 'True' events and gives estimates for the success
-- probability.  'True' is interpreted as a one, and 'False'
-- is interpreted as a zero.
data Summary = S {-# UNPACK #-} !Int  -- sample size
                 {-# UNPACK #-} !Int  -- number of successes

instance Show Summary where
    show s@(S n c) =
        printf "    sample size: %d" n
        ++ printf "\n      successes: %g" c
        ++ printf "\n     proportion: %g" (sampleMean s)
        ++ printf "\n             SE: %g" (sampleSE s)
        ++ printf "\n         99%% CI: (%g, %g)" c1 c2
      where (c1,c2) = sampleCI 0.99 s

instance Monoid Summary where
    mempty = empty
    mappend = union

instance NFData Summary where
    rnf s = s `seq` ()

-- | Get a summary of a list of values.
summary :: [Bool] -> Summary
summary = foldl' update empty

-- | Get an empty summary.
empty :: Summary
empty = S 0 0

-- | Take the union of two summaries.
union :: Summary -> Summary -> Summary
union (S na ca) (S nb cb) = S (na + nb) (ca + cb)

-- | Update the summary with a data point.
update :: Summary -> Bool -> Summary
update (S n c) i =
    let n' = n+1
        c' = if i then c+1 else c
    in S n' c'

-- | Get the sample size.
sampleSize :: Summary -> Int
sampleSize (S n _) = n

-- | Get the number of 'True' values
count :: Summary -> Int
count (S _ c) = c

-- | Get the proportion of 'True' events.
sampleMean :: Summary -> Double
sampleMean (S n c) = fromIntegral c / fromIntegral n

-- | Get the standard error for the sample proportion.
sampleSE :: Summary -> Double
sampleSE s = sqrt (p*(1-p) / n)
  where p = sampleMean s
        n = fromIntegral $ sampleSize s

-- | Get a Central Limit Theorem-based confidence interval for the mean
-- with the specified coverage level.  The level must be in the range @(0,1)@.
sampleCI :: Double -> Summary -> (Double,Double)
sampleCI level s = interval level (sampleMean s) (sampleSE s)
