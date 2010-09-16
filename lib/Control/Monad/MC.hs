-----------------------------------------------------------------------------
-- |
-- Module     : Control.Monad.MC
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
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
