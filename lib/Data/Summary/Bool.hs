{-# LANGUAGE DeriveDataTypeable #-}
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
    -- * Summary type
    Summary,

    -- * Properties
    size,
    sum,
    mean,
    meanSE,
    meanCI,

    -- * Construction
    empty,
    singleton,

    -- * Insertion
    insert,
    insertWith,

    -- * Combine
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

import Prelude hiding (sum)
import Data.List( foldl' )
import Data.Monoid( Monoid(..) )
import Data.Data( Data, Typeable )
import Text.Printf( printf )

import Data.Summary.Utils( interval )


-- | A type for storing summary statistics for a data set of
-- booleans.  Specifically, this just keeps track of the number
-- of 'True' events and gives estimates for the success
-- probability.  'True' is interpreted as a one, and 'False'
-- is interpreted as a zero.
data Summary = S {-# UNPACK #-} !Int -- number of observations
                 {-# UNPACK #-} !Int -- number of True values
    deriving(Eq, Data, Typeable)

instance Show Summary where
    show s@(S n x) =
             printf "    sample size: %d" n
        ++ printf "\n      successes: %d" x
        ++ printf "\n     proportion: %g" (mean s)
        ++ printf "\n             SE: %g" (meanSE s)
        ++ printf "\n         99%% CI: (%g, %g)" c1 c2
      where (c1,c2) = meanCI 0.99 s

instance Monoid Summary where
    mempty = empty
    mappend = union

-- | Number of observations.
size :: Summary -> Int
size (S n _) = n

-- | Number of 'True' values.
sum :: Summary -> Int
sum (S _ x) = x

-- | Proportion of 'True' values.
mean :: Summary -> Double
mean (S n x) = fromIntegral x / fromIntegral n

-- | Standard error for the mean (proportion of 'True' values).
meanSE :: Summary -> Double
meanSE s = sqrt (p*(1-p) / n)
  where p = mean s
        n = fromIntegral $ size s

-- | Central Limit Theorem based confidence interval for the
-- population mean (proportion) at the specified coverage level.  The
-- level must be in the range @(0,1)@.
meanCI :: Double -> Summary -> (Double,Double)
meanCI level s = interval level (mean s) (meanSE s)

-- | Get an empty summary.
empty :: Summary
empty = S 0 0

-- | Summarize a single value.
singleton :: Bool -> Summary
singleton x = S 1 (if x then 1 else 0)

-- | Update the summary with a data point.
insert :: Bool -> Summary -> Summary
insert y (S n x) =
    let n' = n+1
        x' = if y then x+1 else x
    in S n' x'

-- | Apply a function and update the summary with the result.
insertWith :: (a -> Bool) -> a -> Summary -> Summary
insertWith f a = insert (f a)

-- | Take the union of two summaries.
union :: Summary -> Summary -> Summary
union (S na xa) (S nb xb) = S (na + nb) (xa + xb)

-- | Take the union of a list of summaries.
unions :: [Summary] -> Summary
unions = foldl' union empty

-- | Get a summary of a list of values.
fromList :: [Bool] -> Summary
fromList = foldl' (flip insert) empty

-- | Map a function over a list of values and summarize the results.
fromListWith :: (a -> Bool) -> [a] -> Summary
fromListWith f = fromList . map f

-- | Convert to (size,sum).
toStats :: Summary -> (Int,Int)
toStats (S n x) = (n,x)

-- | Convert from (size,sum).  No validation is performed.
fromStats :: Int -> Int -> Summary
fromStats = S
