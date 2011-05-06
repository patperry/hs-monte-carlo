-----------------------------------------------------------------------------
-- |
-- Module     : Control.Monad.MC.Repeat
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
-- Stability  : experimental
--

module Control.Monad.MC.Repeat (
    -- * Repeating computations
    repeatMC,
    replicateMC,
    ) where

import Control.Monad.MC.Base

-- | Produce a lazy infinite list of values from the given Monte Carlo
-- generator.
repeatMC :: (MonadMC m) => m a -> m [a]
repeatMC = interleaveSequence . repeat
{-# INLINE repeatMC #-}

-- | Produce a lazy list of the given length using the specified
-- generator.
replicateMC :: (MonadMC m) => Int -> m a -> m [a]
replicateMC n = interleaveSequence . replicate n
{-# INLINE replicateMC #-}

interleaveSequence :: (MonadMC m) => [m a] -> m [a]
interleaveSequence []     = return []
interleaveSequence (m:ms) = unsafeInterleaveMC $ do
    a  <- m
    as <- interleaveSequence ms
    return (a:as)
{-# INLINE interleaveSequence #-}
