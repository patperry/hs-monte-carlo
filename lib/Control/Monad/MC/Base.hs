{-# LANGUAGE TypeFamilies, MultiParamTypeClasses, FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Control.Monad.MC.Base
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

module Control.Monad.MC.Base
    where

import Control.Monad
import qualified Control.Monad.MC.GSLBase as GSL

class HasRNG m where
    -- | The random number generator type for the monad.
    type RNG m

class (Monad m, HasRNG m) => MonadMC m where
    -- | Get the current random number generator.
    getRNG :: m (RNG m)
    
    -- | Set the current random number generator.
    setRNG :: RNG m -> m ()
    
    -- | @uniform a b@ generates a value uniformly distributed in @[a,b)@.
    uniform :: Double -> Double -> m Double
    
    -- | @uniformInt n@ generates an integer uniformly in the range @[0,n-1]@.
    -- It is an error to call this function with a non-positive value.
    uniformInt :: Int -> m Int
    
    -- | @normal mu sigma@ generates a Normal random variable with mean
    -- @mu@ and standard deviation @sigma@.
    normal :: Double -> Double -> m Double

    -- | @exponential mu@ generates an Exponential variate with mean @mu@.
    exponential :: Double -> m Double

    -- | @levy c alpha@ gets a Levy alpha-stable variate with scale @c@ and
    -- exponent @alpha@.  The algorithm only works for @0 < alpha <= 2@.
    levy :: Double -> Double -> m Double

    -- | @levySkew c alpha beta @ gets a skew Levy alpha-stable variate 
    -- with scale @c@, exponent @alpha@, and skewness @beta@.  The skew
    -- parameter must lie in the range @[-1,1]@.  The algorithm only works
    -- for @0 < alpha <= 2@.
    levySkew :: Double -> Double -> Double -> m Double
    
    -- | @poisson mu@ generates a Poisson random variable with mean @mu@.
    poisson :: Double -> m Int
    
    -- | Get the baton from the Monte Carlo monad without performing any
    -- computations.  Useful but dangerous.
    unsafeInterleaveMC :: m a -> m a


-- | Generate 'True' events with the given probability
bernoulli :: (MonadMC m) => Double -> m Bool
bernoulli p = liftM (< p) $ uniform 0 1
{-# INLINE bernoulli #-}

------------------------------- Instances -----------------------------------

instance HasRNG GSL.MC where
    type RNG GSL.MC = GSL.RNG

instance MonadMC GSL.MC where
    getRNG = GSL.getRNG
    {-# INLINE getRNG #-}
    setRNG = GSL.setRNG
    {-# INLINE setRNG #-}
    uniform = GSL.uniform
    {-# INLINE uniform #-}
    uniformInt = GSL.uniformInt
    {-# INLINE uniformInt #-}
    normal = GSL.normal
    {-# INLINE normal #-}
    exponential = GSL.exponential
    {-# INLINE exponential #-}
    levy = GSL.levy
    {-# INLINE levy #-}
    levySkew = GSL.levySkew
    {-# INLINE levySkew #-}
    poisson = GSL.poisson
    {-# INLINE poisson #-}
    unsafeInterleaveMC = GSL.unsafeInterleaveMC
    {-# INLINE unsafeInterleaveMC #-}

instance (Monad m) => HasRNG (GSL.MCT m) where
    type RNG (GSL.MCT m) = GSL.RNG

instance (Monad m) => MonadMC (GSL.MCT m) where
    getRNG = GSL.liftMCT GSL.getRNG
    {-# INLINE getRNG #-}
    setRNG r = GSL.liftMCT $ GSL.setRNG r
    {-# INLINE setRNG #-}
    uniform a b = GSL.liftMCT $ GSL.uniform a b
    {-# INLINE uniform #-}
    uniformInt n = GSL.liftMCT $ GSL.uniformInt n
    {-# INLINE uniformInt #-}
    normal mu sigma = GSL.liftMCT $ GSL.normal mu sigma
    {-# INLINE normal #-}
    exponential mu = GSL.liftMCT $ GSL.exponential mu
    {-# INLINE exponential #-}    
    levy c alpha = GSL.liftMCT $ GSL.levy c alpha
    {-# INLINE levy #-}
    levySkew c alpha beta = GSL.liftMCT $ GSL.levySkew c alpha beta
    {-# INLINE levySkew #-}
    poisson mu = GSL.liftMCT $ GSL.poisson mu
    {-# INLINE poisson #-}
    unsafeInterleaveMC = GSL.unsafeInterleaveMCT
    {-# INLINE unsafeInterleaveMC #-}
