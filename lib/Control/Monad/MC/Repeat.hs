{-# LANGUAGE RankNTypes #-}
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
    foldMC,
    repeatMC,
    replicateMC,
    ) where

import Control.Monad.Primitive( PrimMonad )
import Control.Monad.MC.GSLBase
import Control.Monad.ST( ST, runST )
import Control.Monad.ST.Unsafe( unsafeInterleaveST )


-- | Generate a sequence of replicates and incrementally consume
-- them via a left fold.
--
-- This fold is /not/ strict. The replicate consumer is responsible for
-- forcing the evaluation of its result to avoid space leaks.
foldMC :: (PrimMonad m) => (a -> b -> MC m a) -- ^ Replicate consumer.
                        -> a                  -- ^ Initial state for replicate consumer.
                        -> Int                -- ^ Number of replicates.
                        -> MC m b             -- ^ Generator.
                        -> MC m a
foldMC f a n mb | n <= 0    = return a
                | otherwise = do
    b <- mb
    a' <- f a b
    foldMC f a' (n-1) mb
{-# INLINE foldMC #-}


-- | Produce a lazy infinite list of replicates from the given random
-- number generator and Monte Carlo procedure.
repeatMC :: (forall s. STMC s a) -> (forall s. ST s (STRNG s)) -> [a]
repeatMC mc mrng = runST $ do
    rng <- mrng
    go $ runMC mc rng
  where
    go m = unsafeInterleaveST $ do
        a  <- m
        as <- go m
        return (a:as)


-- | Produce a lazy list of the given length using the specified
-- random number genrator and Monte Carlo procedure.
replicateMC :: Int -> (forall s. STMC s a) -> (forall s. ST s (STRNG s)) -> [a]
replicateMC n mc mrng = take n $ repeatMC mc mrng

