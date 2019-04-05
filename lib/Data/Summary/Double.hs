{-# LANGUAGE DeriveDataTypeable #-}
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
    -- * Summary type
    Summary,

    -- * Properties
    -- ** Sum
    size,
    sum,
    sumSquaredErrors,
    -- ** Mean
    mean,
    meanSE,
    meanCI,
    -- ** Scale
    stddev,
    variance,
    -- ** Extremes
    maximum,
    minimum,

    -- * Construction
    empty,
    singleton,

    -- * Insertion
    insert,
    insertWith,

    -- * Combination
    union,
    unions,

    -- * Conversion
    -- ** Lists
    fromList,
    fromListWith,

    -- ** Statistics
    toStats,
    fromStats,

    ) where

import Prelude hiding (maximum, minimum, sum)
import qualified Prelude as P

import Data.Data( Data, Typeable )
import Data.List( foldl' )
import Data.Monoid( Monoid(..) )
import Text.Printf

import Data.Summary.Utils( interval )


-- | A type for storing summary statistics for a data set including
-- sample size, min and max values, and first and second moments.
data Summary = S {-# UNPACK #-} !Int     -- sample size
                 {-# UNPACK #-} !Double  -- sample mean
                 {-# UNPACK #-} !Double  -- sum of squares
                 {-# UNPACK #-} !Double  -- sample min
                 {-# UNPACK #-} !Double  -- sample max
    deriving(Eq, Data, Typeable)

instance Show Summary where
    show s@(S n mu _ l h) =
             printf "    sample size: %d" n
        ++ printf "\n        minimum: %g" l
        ++ printf "\n        maximum: %g" h
        ++ printf "\n           mean: %g" mu
        ++ printf "\n             SE: %g" (meanSE s)
        ++ printf "\n         99%% CI: (%g, %g)" c1 c2
      where (c1,c2) = meanCI 0.99 s

instance Semigroup Summary where
  (<>) = union

instance Monoid Summary where
    mempty = empty
    mappend = union

-- | Number of observations.
size :: Summary -> Int
size (S n _ _ _ _) = n

-- | Sum of values.
sum :: Summary -> Double
sum s = (fromIntegral $ size s) * (mean s)

-- | Mean value.
mean :: Summary -> Double
mean (S _ m _ _ _) = m

-- | Standard error of the mean.
meanSE :: Summary -> Double
meanSE s = sqrt (variance s / fromIntegral (size s))

-- | Get a Central Limit Theorem based confidence interval for the
-- population mean with the specified coverage level.  The level must
-- be in the range @(0,1)@.
meanCI :: Double -> Summary -> (Double,Double)
meanCI level s = interval level (mean s) (meanSE s)

-- | Sample standard deviation.
stddev :: Summary -> Double
stddev s = sqrt (variance s)

-- | Sample variance.
variance :: Summary -> Double
variance s = (sumSquaredErrors s) / fromIntegral (size s - 1)

-- | Sum of squared errors @(x[i] - mean)^2@.
sumSquaredErrors :: Summary -> Double
sumSquaredErrors (S _ _ s _ _) = s

-- | Maximum value.
maximum :: Summary -> Double
maximum (S _ _ _ _ h) = h

-- | Minimum value.
minimum :: Summary -> Double
minimum (S _ _ _ l _) = l

-- | An empty summary.
empty :: Summary
empty = S 0 0 0 (1/0) (-1/0)

-- | Summarize a single value.
singleton :: Double -> Summary
singleton x = S 1 x 0 x x

-- | Update the summary with a data point.
-- Running mean and variance computed as in Knuth, Vol 2, page 232,
-- 3rd edition, see <http://www.johndcook.com/standard_deviation.html> for
-- a description.
insert :: Double -> Summary -> Summary
insert x (S n m s l h) =
    let n'    = n+1
        delta = x - m
        m'    = m + delta / fromIntegral n'
        s'    = s + delta*(x - m')
        l'    = if x < l then x else l
        h'    = if x > h then x else h
    in S n' m' s' l' h'

-- | Apply a function and update the summary with the result.
insertWith :: (a -> Double) -> a -> Summary -> Summary
insertWith f a = insert (f a)

-- | Take the union of two summaries.
-- Use the updating rules from Chan et al. \"Updating Formulae and a Pairwise
--   Algorithm for Computing Sample Variances,\" available at
--   <http://infolab.stanford.edu/pub/cstr/reports/cs/tr/79/773/CS-TR-79-773.pdf>.
union :: Summary -> Summary -> Summary
union (S na ma sa la ha) (S nb mb sb lb hb) =
    let delta = mb - ma
        (na', nb') = (fromIntegral na, fromIntegral nb)
        n  = na + nb
        n' = fromIntegral n
        weightedDelta = delta*nb'/n'
        m  | n == 0    = 0
           | otherwise = ma + weightedDelta
        s  | n == 0    = 0
           | otherwise = sa + sb + delta*na'*weightedDelta
        l  = P.min la lb
        h  = P.max ha hb
    in S n m s l h

-- | Take the union of a list of summaries.
unions :: [Summary] -> Summary
unions = foldl' union empty

-- | Get a summary of a list of values.
fromList :: [Double] -> Summary
fromList = foldl' (flip insert) empty

-- | Map a function over a list of values and summarize the results.
fromListWith :: (a -> Double) -> [a] -> Summary
fromListWith f = fromList . map f

-- | Convert to (size, mean, sumSquaredErrors, minimum, maximum).
toStats :: Summary -> (Int,Double,Double,Double,Double)
toStats (S n m s l u) = (n,m,s,l,u)

-- | Convert from (size, mean, sumSquaredErrors, minimum, maximum).
-- No validation is performed.
fromStats :: Int -> Double -> Double -> Double -> Double -> Summary
fromStats = S

