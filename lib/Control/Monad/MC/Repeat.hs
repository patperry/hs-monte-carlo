-----------------------------------------------------------------------------
-- |
-- Module     : Control.Monad.MC.Repeat
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

module Control.Monad.MC.Repeat (
    -- * Averaging functions
    repeatMC,
    repeatMCWith,
    ) where

import Control.Monad
import Control.Monad.MC.Base
import Data.List( foldl' )

data Summary = S {-# UNPACK #-} !Int
                 {-# UNPACK #-} !Double
                 {-# UNPACK #-} !Double

toDouble :: Int -> Double
toDouble = realToFrac . toInteger

initSummary :: Summary
initSummary = S 0 0 0

-- | Running mean and variance.  See Knuth, Vol 2, page 232, 3rd edition,
-- or http://www.johndcook.com/standard_deviation.html.
update :: Summary -> Double -> Summary
update (S n m s) x =
    let n'    = n+1
        delta = x - m
        m'    = m + delta / toDouble n'
        s'    = s + delta*(x - m')
    in S n' m' s'

sampleSize :: Summary -> Int
sampleSize (S n _ _) = n

sampleMean :: Summary -> Double
sampleMean (S _ m _) = m

sampleVar :: Summary -> Double
sampleVar (S n _ s) = s / toDouble (n - 1)

sampleSE :: Summary -> Double
sampleSE s = sqrt (sampleVar s) / toDouble (sampleSize s)

-- | Repeat a Monte Carlo generator the given number of times and return
-- the sample mean and standard error.  Note that this only works with
-- @Double@s.
repeatMC :: (MonadMC m)
         => Int
         -> m Double
         -> m (Double,Double)
repeatMC n mb = do
    s <- repeatMCWith update initSummary n mb
    let m  = sampleMean s
        se = sampleSE s
    m `seq` se `seq` return (m,se)
{-# INLINE repeatMC #-}

-- | Generalized version of 'repeatMC'.  Run a Monte Carlo generator
-- the given number of times and accumulate the results.  The accumulator
-- is strictly evaluated.
repeatMCWith :: (MonadMC m)
             => (a -> b -> a) -- ^ accumulator
             -> a             -- ^ initial value
             -> Int           -- ^ number of repetitions
             -> m b           -- ^ generator
             -> m a
repeatMCWith f a n mb = do
    bs <- interleaveSequence $ replicate n mb
    return $! foldl' f a bs
{-# INLINE repeatMCWith #-}


interleaveSequence :: (MonadMC m) => [m a] -> m [a]
interleaveSequence []     = return []
interleaveSequence (m:ms) = unsafeInterleaveMC $ do
    a  <- m
    as <- interleaveSequence ms
    return (a:as)
{-# INLINE interleaveSequence #-}
