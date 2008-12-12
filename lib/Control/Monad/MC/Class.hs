-----------------------------------------------------------------------------
-- |
-- Module     : Control.Monad.MC.Class
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
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
    poisson,
    
    -- * Interleaving computations
    unsafeInterleaveMC
    ) where

import Control.Monad.MC.Base
