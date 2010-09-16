-----------------------------------------------------------------------------
-- |
-- Module     : Control.Monad.MC
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--
-- A monad and monad transformer for monte carlo computations.  Currently,
-- the default is the GNU Scientific Library-based implementation, but this
-- may change in the future.
--

module Control.Monad.MC (
    module Control.Monad.MC.GSL
    ) where

import Control.Monad.MC.GSL
