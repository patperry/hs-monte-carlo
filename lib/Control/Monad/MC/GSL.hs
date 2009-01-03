-----------------------------------------------------------------------------
-- |
-- Module     : Control.Monad.MC.GSL
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

module Control.Monad.MC.GSL (
    -- * The Monte Carlo monad
    MC,
    runMC,
    evalMC,
    execMC,
    
    -- * The Monte Carlo monad transformer
    MCT,
    runMCT,
    evalMCT,
    execMCT,

    -- * Pure random number generator creation
    RNG,
    mt19937,
    mt19937WithState,
    rngName,
    rngSize,
    rngState,

    -- * Overloaded Monte Carlo monad interface
    module Control.Monad.MC.Class,

    ) where

import Control.Monad.MC.GSLBase ( MC, runMC, evalMC, execMC,
    MCT, runMCT, evalMCT, execMCT, RNG, mt19937, mt19937WithState,
    rngName, rngSize, rngState )
import Control.Monad.MC.Class hiding ( RNG )
