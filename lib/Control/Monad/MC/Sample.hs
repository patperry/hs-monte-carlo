-----------------------------------------------------------------------------
-- |
-- Module     : Control.Monad.MC.Sample
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
-- Stability  : experimental
--

module Control.Monad.MC.Sample (
    -- * Sampling from lists
    sample,
    sampleWithWeights,
    sampleSubset,
    sampleSubsetWithWeights,

    -- * Sampling @Int@s
    sampleInt,
    sampleIntWithWeights,
    sampleIntSubset,
    sampleIntSubsetWithWeights,

    -- * Shuffling
    shuffle,
    ) where

import Control.Monad( forM_, liftM )
import Control.Monad.Primitive( PrimMonad )
import Control.Monad.Trans.Class( lift )
import Data.List( foldl', sort )
import qualified Data.Vector as BV
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV

import Control.Monad.MC.GSLBase
import Control.Monad.MC.Walker


-- | @sample xs@ samples a value uniformly from the elements of @xs@.  The
-- results are undefined if @length xs@ is zero.
sample :: (PrimMonad m) => [a] -> MC m a
sample xs = let
    n = length xs
    in sampleHelp n xs $ sampleInt n
{-# INLINE sample #-}


-- | @sampleWithWeights wxs@ samples a value from the list with the given
-- weight.
sampleWithWeights :: (PrimMonad m) => [(Double, a)] -> MC m a
sampleWithWeights wxs = let
    (ws,xs) = unzip wxs
    n       = length xs
    in sampleHelp n xs $ sampleIntWithWeights ws n
{-# INLINE sampleWithWeights #-}


sampleHelp :: (PrimMonad m) => Int -> [a] -> MC m Int -> MC m a
sampleHelp _n xs f = let
    arr = BV.fromList xs
    in liftM (BV.unsafeIndex arr) f
{-# INLINE sampleHelp #-}


-- | @sampleSubset xs k@ samples a subset of size @k@ from @xs@ by
-- sampling without replacement.  The return value is a list of length @k@
-- with the elements in the subset in the order that they were sampled.
sampleSubset :: (PrimMonad m) => [a] -> Int -> MC m [a]
sampleSubset xs k = let
    n = length xs
    in sampleSubsetHelp n xs $ sampleIntSubset n k
{-# INLINE sampleSubset #-}


-- | Sample a subset of the elements with the given weights.  Return
-- the elements of the subset in the order they were sampled.
sampleSubsetWithWeights :: (PrimMonad m) => [(Double,a)] -> Int -> MC m [a]
sampleSubsetWithWeights wxs k = let
    (ws,xs) = unzip wxs
    n = length ws
    in sampleSubsetHelp n xs $ sampleIntSubsetWithWeights ws n k
{-# INLINE sampleSubsetWithWeights #-}


sampleSubsetHelp :: (Monad m) => Int -> [a] -> m [Int] -> m [a]
sampleSubsetHelp _n xs f = let
    arr = BV.fromList xs
    in liftM (map $ BV.unsafeIndex arr) f
{-# INLINE sampleSubsetHelp #-}


-- | @sampleInt n@ samples integers uniformly from @[ 0..n-1 ]@.  It is an
-- error to call this function with a non-positive @n@.
sampleInt :: (PrimMonad m) => Int -> MC m Int
sampleInt n | n < 1     = fail "invalid argument"
            | otherwise = uniformInt n


-- | @sampleIntWithWeights ws n@ samples integers from @[ 0..n-1 ]@ with the
-- probability of choosing @i@ proportional to @ws !! i@.  The list @ws@ must
-- have length equal to @n@.  Also, the elements of @ws@ must be non-negative
-- with at least one nonzero entry.
sampleIntWithWeights :: (PrimMonad m) => [Double] -> Int -> MC m Int
sampleIntWithWeights ws n =
    let qjs = computeTable n ws
    in liftM (indexTable qjs) (uniform 0 1)


-- | @sampleIntSubset n k@ samples a subset of size @k@ by sampling without
-- replacement from the integers @{ 0, ..., n-1 }@.  The return value is a
-- list of length @k@ with the elements in the subset in the order that they
-- were sampled.
sampleIntSubset :: (PrimMonad m) => Int -> Int -> MC m [Int]
sampleIntSubset n k | k < 0     = fail "negative subset size"
                    | k > n     = fail "subset size is too big"
                    | otherwise = do
    xs <- lift $ (V.thaw . V.fromList) [ 0..n-1 ]
    go xs [] n k
  where
    go xs ys n' k' | k' == 0   = return $ reverse ys
                   | otherwise = do
        u <- uniformInt n'
        y <- lift $ do
                 i <- MV.unsafeRead xs u
                 j <- MV.unsafeRead xs (n'-1)
                 MV.unsafeWrite xs u j
                 return i
        go xs (y:ys) (n'-1) (k'-1)


-- | @sampleIntSubsetWithWeights ws n k@ samplea size @k@ subset of
-- @{ 0, ..., n-1 }@ with the given weights by sampling elements without
-- replacement.  It returns the elements of the subset in the order
-- they were sampled.
sampleIntSubsetWithWeights :: (PrimMonad m) => [Double] -> Int -> Int -> MC m [Int]
sampleIntSubsetWithWeights ws n k | k < 0     = fail "negative subset size"
                                  | k > n     = fail "subset size is too big"
                                  | otherwise = let
    wsum = foldl' (+) 0 $ take n ws
    wjs  = [ (w / wsum, j) | (w,j) <- reverse $ sort $ zip ws [ 0..n-1 ] ]
    in do
        xs <- lift $ (V.thaw . V.fromList) wjs
        go xs wsum [] n k
  where
    go xs wsum' ys n' k' | k' == 0   = return $ reverse ys
                         | otherwise = do
        target <- uniform 0 wsum'
        (w,y) <- lift $ do
                     (i,wj) <- findTarget xs n' target 0 0
                     shiftDown xs (i+1) (n'-1)
                     return wj
        let wsum'' = wsum' - w
            ys'    = y:ys
            n''    = n' - 1
            k''    = k' - 1
        go xs wsum'' ys' n'' k''

    findTarget xs n' target i acc
        | i == n' - 1 = do
            wj <- MV.unsafeRead xs i
            return (i,wj)
        | otherwise = do
            (w,j) <- MV.unsafeRead xs i
            let acc' = acc + w
            if target <= acc'
                then return (i,(w,j))
                else findTarget xs n' target (i+1) acc'

    shiftDown xs from to =
        forM_ [ from..to ] $ \i -> do
            wj <- MV.unsafeRead xs i
            MV.unsafeWrite xs (i-1) wj


-- | @shuffle xs@ randomly permutes the list @xs@ and returns
-- the result.  All permutations of the elements of @xs@ are equally
-- likely.
shuffle :: (PrimMonad m) => [a] -> MC m [a]
shuffle xs = let
    n   = length xs
    mis = liftM BV.fromList (sampleIntSubset n n)
    in liftM (BV.toList . BV.unsafeBackpermute (BV.fromList xs)) mis

