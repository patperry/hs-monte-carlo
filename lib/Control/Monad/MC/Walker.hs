{-# LANGUAGE TypeOperators #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Control.Monad.MC.Walker
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--
-- An implementation of Walker's Alias method for sampling from discrete
-- distributions.  See section III.4 of Luc Devroye's book
-- "Non-Uniform Random Variate Generation", which is available on his
-- homepage, for a description of how it works.
module Control.Monad.MC.Walker (
    Table,
    computeTable,
    indexTable,
    tableSize,
    component,
    ) where

import Control.Monad
import Control.Monad.ST
import Data.Array.Vector

-- | The table, which represents an equiprobable mixture of two-point
-- distributions.  The @l@th entry of the table represents a mixture
-- distribution with weight @q[l]@ on @l@ and weight @(1-q[l])@ on @j[l]@.
-- The @l@th element of the table stores the pair @q[l] :*: j[l]@.
newtype Table = T (UArr (Double :*: Int))

-- | Get the @i@th mixture component.  That is, return @q[i]@ and @j[i]@,
-- where the @i@th mixture component puts mass @q[i]@ on @i@ and mass
-- @1 - q[i]@ on @j[i]@.
component :: Table -> Int -> (Double,Int)
component (T qjs) i = let
    (q' :*: j) = indexU qjs i
    q = q' - fromIntegral i
    in (q,j)

-- | Compute the table for use in Walker's aliasing method.
computeTable :: Int -> [Double] -> Table
computeTable n ws = runST $ do
    (qjs, sets) <- initTable n ws
    breakLarger qjs sets
    scaleTable qjs
    liftM T $ unsafeFreezeAllMU qjs

-- | Given an alias table and a number in the range [0,1),
-- get the corresponding sample in the table.
indexTable :: Table -> Double -> Int
indexTable (T qjs) u = let
    n  = lengthU qjs
    nu = u * fromIntegral n
    l  = floor nu
    (ql :*: jl) = indexU qjs l
    in if nu < ql then l else jl

-- | Get the size of the table
tableSize :: Table -> Int
tableSize (T qjs) = lengthU qjs

-- | An intermediate result for use in computing a Table.
type STTable s = MUArr (Double :*: Int) s

-- | A partition of indices into the sets /Greater/ and /Smaller/.  The
-- indices of the /Smaller/ set are stored in positions @0, ..., numSmall - 1@,
-- and the indices of the /Greater/ set are stored in positions
-- @numSmall, ..., n-1@, where @n@ is the size of the underlying array.
data STPartition s = P !(MUArr Int s)
                       !Int

-- | Given a list of weights, @ws@, compute corresponding probabilities, @ps@,
-- and store @map (n*) ps@ in the @qs@ array.  Partition the probabilities
-- into two sets, /Greater/, and /Smaller/ based on whether or not
-- @q >= 1@ or @q < 1@.
initTable :: Int -> [Double] -> ST s (STTable s, STPartition s)
initTable n ws = do
    when (n < 0) $ fail "negative table size"
    sets <- newMU n :: ST s (MUArr Int s)
    qjs  <- newMU n :: ST s (MUArr (Double :*: Int) s)

    -- Store the weights in the table and compute their total.
    total <-
        foldM (\current (i,w) -> do
                  if w >= 0
                      then do
                          writeMU qjs i (w :*: i)
                          return $! current + w
                      else
                          fail $ "negative probability" )
              0
              (zip [0 .. n-1] ws)

    when (total == 0) $ fail "no positive probabilities given"

    -- scale the weights to get the qs, and partition the probabilites
    -- into the two sets
    let scale = fromIntegral n / total
    nsmall <- liftM fst $
        foldM (\(smaller,greater) i -> do
               p <- liftM fstS $ readMU qjs i
               let q = scale*p
               writeMU qjs i (q :*: i)
               if q < 1
                   then do
                       writeMU sets smaller i
                       return (smaller+1,greater)
                   else do
                       writeMU sets greater i
                       return (smaller,greater-1) )
              (0,n-1)
              [0 .. n-1]

    return $ (qjs, P sets nsmall)


-- Given an initialized table and partition, compute the two-point
-- distributions by splitting the larger probabilites sccross multiple
-- distribions.
breakLarger :: STTable s -> STPartition s -> ST s ()
breakLarger qjs (P sets nsmall) | nsmall == 0 = return ()
                                | otherwise   = let
    n = lengthMU qjs
    breakLargerHelp nsmall' i | nsmall' == n = return ()
                              | i == n       = return ()
                              | otherwise    = do
        -- while Greater is not empty
        -- choose k from Greater, l from Smaller
        k  <- readMU sets $ nsmall'
        l  <- readMU sets $ i
        qk <- liftM fstS $ readMU qjs k
        ql <- liftM fstS $ readMU qjs l

        -- set jl := k, finalize (ql,jl)
        let jl = k
        writeMU qjs l (ql :*: jl)

        -- set qk := qk - (1-ql)
        let qk' = qk - (1-ql)
        writeMU qjs k (qk' :*: k)

        -- if qk' < 1, move k from Greater to Smaller
        let nsmall'' = if qk' < 1 then nsmall'+1 else nsmall'

        breakLargerHelp nsmall'' (i+1)
    in
        breakLargerHelp nsmall 0

-- Scale the probabilities in the table so that the lth entry
-- stores q[l] + l instead of q[l].  This helps when we are sampling
-- from the table.
scaleTable :: STTable s -> ST s ()
scaleTable qjs = let
    n = lengthMU qjs in
    forM_ [ 0..(n-1) ] $ \l -> do
        (ql :*: jl) <- readMU qjs l
        writeMU qjs l ((ql + fromIntegral l) :*: jl)

