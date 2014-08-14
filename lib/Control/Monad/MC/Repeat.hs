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


foldMC :: (PrimMonad m) => (a -> b -> MC m a) -> a -> Int -> MC m b -> MC m a
foldMC f a n mb | n <= 0    = return a
                | otherwise = do
    b <- mb
    a' <- f a b
    a' `seq` foldMC f a' (n-1) mb
{-# INLINE foldMC #-}

-- | Produce a lazy infinite list of values from the given seed and
-- Monte Carlo generator.
repeatMC :: (forall s. ST s (STRNG s)) -> (forall s. STMC s a) -> [a]
repeatMC mrng mc = runST $ do
    rng <- mrng
    go $ runMC mc rng
  where
    go m = unsafeInterleaveST $ do
        a  <- m
        as <- go m
        return (a:as)

-- | Produce a lazy list of the given length using the specified seed
-- and Monte Carlo generator.
replicateMC :: (forall s. ST s (STRNG s)) -> Int -> (forall s. STMC s a) -> [a]
replicateMC mrng n mc = take n $ repeatMC mrng mc
