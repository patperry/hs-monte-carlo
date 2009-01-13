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
    replicateMC,
    replicateMCWith,
    
    module Control.Monad.MC.Summary,
    ) where

import Control.Monad
import Control.Monad.MC.Base
import Control.Monad.MC.Summary
import Data.List( foldl' )


-- | Repeat a Monte Carlo generator the given number of times and return
-- the sample summary statistics.  Note that this only works with
-- @Double@s.
replicateMC :: (MonadMC m)
            => Int
            -> m Double
            -> m Summary
replicateMC = replicateMCWith update summary
{-# INLINE replicateMC #-}

-- | Generalized version of 'replicateMC'.  Run a Monte Carlo generator
-- the given number of times and accumulate the results.  The accumulator
-- is strictly evaluated.
replicateMCWith :: (MonadMC m)
                => (a -> b -> a) -- ^ accumulator
                -> a             -- ^ initial value
                -> Int           -- ^ number of repetitions
                -> m b           -- ^ generator
                -> m a
replicateMCWith f a n mb = do
    bs <- interleaveSequence $ replicate n mb
    return $! foldl' f a bs
{-# INLINE replicateMCWith #-}


interleaveSequence :: (MonadMC m) => [m a] -> m [a]
interleaveSequence []     = return []
interleaveSequence (m:ms) = unsafeInterleaveMC $ do
    a  <- m
    as <- interleaveSequence ms
    return (a:as)
{-# INLINE interleaveSequence #-}
