module Main
    where

import Control.Monad
import Control.Monad.Primitive( PrimMonad )
import Control.Monad.ST( runST )
import Data.List( foldl' )
import Text.Printf( printf )

import Control.Monad.MC
import Data.Summary( inInterval )
import Data.Summary.Double

-- | Sample from a binomial distribution with the given parameters.
binomial :: (PrimMonad m) => Int -> Double -> MC m Int
binomial n p = let
    q     = 1 - p
    probs = map (\i -> (fromIntegral $ n `choose` i) * p^^i * q^^(n-i)) [0..n]
    in sampleIntWithWeights probs (n+1)

-- | Get a sample confidence interval for the mean after @reps@ replications of
-- a binomial with the given parameters.
binomialMean :: (PrimMonad m) => Int -> Double -> Int -> MC m (Double,Double)
binomialMean n p reps =
    liftM (sampleCI 0.95) $
        foldMC (\stats x -> return $! update stats (fromIntegral x)) empty
               reps (binomial n p)

-- | Compute @reps@ 95% confidence intervals for the mean of an @(n,p)@
-- binormal based on samples of the given size, and record the number
-- of intervals that contain the true mean.
coverage :: (PrimMonad m) => Int -> Double -> Int -> Int -> MC m Int
coverage n p size reps =
    foldMC (\tot ci -> return $! update tot (mu `inInterval` ci)) 0
           reps (binomialMean n p size)
  where
    mu = fromIntegral n * p
    update tot b = case b of True  -> tot + 1
                             False -> tot

main =
    let seed = 0
        reps = 10000
        n    = 10
        p    = 0.2
        size = 500
        c    = runST $ mt19937 seed >>= runMC (coverage n p size reps)
    in
        printf "\nOf %d 95%%-intervals, %d contain the true value.\n" reps c


---------------------------   Utility functions -----------------------------

factorial :: Int -> Int
factorial n | n <= 0    = 1
            | otherwise = n * factorial (n-1)

choose :: Int -> Int -> Int
choose n k = factorial n `div` (factorial (n-k) * factorial k)
