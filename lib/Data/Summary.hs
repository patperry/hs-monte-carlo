-----------------------------------------------------------------------------
-- |
-- Module     : Data.Summary
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
-- Stability  : experimental
--
-- Utilities for data summaries.
--

module Data.Summary (
    interval,
    inInterval,
    ) where

import GSL.Random.Dist( ugaussianPInv )


-- | Get a Central Limit Theorem based confidence interval for a
-- population parameter with the specified coverage level.  The level
-- must be in the range @(0,1)@.
interval :: Double -- ^ Confidence level.
         -> Double -- ^ Estimate.
         -> Double -- ^ Standard error.
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
