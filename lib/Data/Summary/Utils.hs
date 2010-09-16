-----------------------------------------------------------------------------
-- |
-- Module     : Data.Summary.Utils
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--
-- Utilities for data summaries.
--

module Data.Summary.Utils (
    interval,
    inInterval,
    ) where

import GSL.Random.Dist( ugaussianPInv )

-- | Get a Central Limit Theorem-based confidence interval for the
-- population mean with the specified coverage level.  The level must
-- be in the range @(0,1)@.
interval :: Double -- ^ the confidence level
         -> Double -- ^ the sample mean
         -> Double -- ^ the sample standard error
         -> (Double,Double)
interval level xbar se | not (level > 0 && level < 1) = 
                             error "level must be between 0 and 1"
                       | otherwise =
    let alpha = (0.5 - level) + 0.5
        z     = -(ugaussianPInv (0.5*alpha))
        delta = z*se
    in (xbar-delta, xbar+delta)

-- | Tests if the value is in the open interval (a,b)
inInterval :: Double -> (Double,Double) -> Bool
x `inInterval` (a,b) = x > a && x < b
