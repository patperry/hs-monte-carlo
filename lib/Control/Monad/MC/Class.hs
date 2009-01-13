-----------------------------------------------------------------------------
-- |
-- Module     : Control.Monad.MC.Class
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--
-- The abstract MonadMC interface and utility functions for Monte Carlo
-- computations.
--

module Control.Monad.MC.Class (
    -- * The Monte Carlo monad type class
    HasRNG(..),
    MonadMC,
    
    -- * Getting and setting the generator
    getRNG,
    setRNG,
    
    -- * Random distributions
    uniform,
    uniformInt,
    normal,
    exponential,
    levy,
    levySkew,
    bernoulli,
    poisson,
    
    module Control.Monad.MC.Sample,
    module Control.Monad.MC.Repeat,
    
    -- * Interleaving computations
    unsafeInterleaveMC
    ) where

import Control.Monad.MC.Base
import Control.Monad.MC.Sample
import Control.Monad.MC.Repeat
