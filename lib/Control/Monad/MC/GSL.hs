-----------------------------------------------------------------------------
-- |
-- Module     : Control.Monad.MC.GSL
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
-- Stability  : experimental
--
-- A monad and monad transformer for monte carlo computations built on top
-- of the functions in the GNU Scientific Library.

module Control.Monad.MC.GSL (
    -- * The Monte Carlo monad
    MC(..),
    STMC,
    IOMC,

    -- * Random number generators
    RNG,
    IORNG,
    STRNG,
    Seed,
    mt19937,
    mt19937WithState,
    getRNGName,
    getRNGSize,
    getRNGState,
    setRNGState,

    -- * Random number distributions
    uniform,
    uniformInt,
    normal,
    exponential,
    levy,
    levySkew,
    poisson,
    cauchy,
    beta,
    logistic,
    pareto,
    weibull,
    gamma,
    multinomial,
    dirichlet,
    bernoulli,

    module Control.Monad.MC.Sample,
    module Control.Monad.MC.Repeat,

    -- * Unsafe operations
    unsafeInterleaveMC,
    ) where

import Control.Monad.MC.GSLBase
import Control.Monad.MC.Sample
import Control.Monad.MC.Repeat
