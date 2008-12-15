-----------------------------------------------------------------------------
-- |
-- Module     : Control.Monad.MC.Repeat
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

module Control.Monad.MC.Repeat (
    -- * Averaging functions
    repeatMC,
    repeatMCWith,
    
    module Control.Monad.MC.Summary,
    ) where

import Control.Monad
import Control.Monad.MC.Base
import Control.Monad.MC.Summary
import Data.List( foldl' )

-- | Repeat a Monte Carlo generator the given number of times and return
-- the sample summary statistics.  Note that this only works with
-- @Double@s.
repeatMC :: (MonadMC m)
         => Int
         -> m Double
         -> m Summary
repeatMC = repeatMCWith update summary
{-# INLINE repeatMC #-}

-- | Generalized version of 'repeatMC'.  Run a Monte Carlo generator
-- the given number of times and accumulate the results.  The accumulator
-- is strictly evaluated.
repeatMCWith :: (MonadMC m)
             => (a -> b -> a) -- ^ accumulator
             -> a             -- ^ initial value
             -> Int           -- ^ number of repetitions
             -> m b           -- ^ generator
             -> m a
repeatMCWith f a n mb = do
    bs <- interleaveSequence $ replicate n mb
    return $! foldl' f a bs
{-# INLINE repeatMCWith #-}


interleaveSequence :: (MonadMC m) => [m a] -> m [a]
interleaveSequence []     = return []
interleaveSequence (m:ms) = unsafeInterleaveMC $ do
    a  <- m
    as <- interleaveSequence ms
    return (a:as)
{-# INLINE interleaveSequence #-}
