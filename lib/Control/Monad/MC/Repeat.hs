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

import Control.Monad.Primitive( PrimMonad )
import Control.Monad.MC.GSLBase

-- | Produce a lazy infinite list of values from the given Monte Carlo
-- generator.
repeatMC :: (PrimMonad m) => MC m a -> MC m [a]
repeatMC = interleaveSequence . repeat

-- | Produce a lazy list of the given length using the specified
-- generator.
replicateMC :: (PrimMonad m) => Int -> MC m a -> MC m [a]
replicateMC n = interleaveSequence . replicate n

interleaveSequence :: (PrimMonad m) => [MC m a] -> MC m [a]
interleaveSequence []     = return []
interleaveSequence (m:ms) = unsafeInterleaveMC $ do
    a  <- m
    as <- interleaveSequence ms
    return (a:as)
