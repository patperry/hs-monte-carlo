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
    sampleSubset',
    sampleSubsetWithWeights,
    sampleSubsetWithWeights',

    -- * Sampling @Int@s
    sampleInt,
    sampleIntWithWeights,
    sampleIntSubset,
    sampleIntSubset',
    sampleIntSubsetWithWeights,
    sampleIntSubsetWithWeights',

    -- * Shuffling
    shuffle,
    shuffleInt,
    shuffleInt',
    ) where

import Control.Monad
import Control.Monad.Primitive( PrimMonad )
import Control.Monad.ST hiding (unsafeInterleaveST)
import Control.Monad.ST.Unsafe (unsafeInterleaveST)
import Control.Monad.MC.GSLBase
import Control.Monad.MC.Repeat
import Control.Monad.MC.Walker
import Data.List( foldl', sort )

import Data.Vector.Unboxed( MVector, Unbox )
import qualified Data.Vector as BV
import qualified Data.Vector.Mutable as BMV
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Generic.Mutable as MV

-- | @sample xs@ samples a value uniformly from the elements of @xs@.  The
-- results are undefined if @length xs@ is zero.
sample :: (PrimMonad m) => [a] -> MC m a
sample xs = let
    n = length xs
    in sampleHelp n xs $ sampleInt n

-- | @sampleWithWeights wxs@ samples a value from the list with the given
-- weight.
sampleWithWeights :: (PrimMonad m) => [(Double, a)] -> MC m a
sampleWithWeights wxs = let
    (ws,xs) = unzip wxs
    n       = length xs
    in sampleHelp n xs $ sampleIntWithWeights ws n

-- | @sampleSubset xs k@ samples a subset of size @k@ from @xs@ by
-- sampling without replacement.  The return value is a list of length @k@
-- with the elements in the subset in the order that they were sampled.  Note
-- also that the elements are lazily generated.
sampleSubset :: (PrimMonad m) => [a] -> Int -> MC m [a]
sampleSubset xs k = let
    n = length xs
    in sampleListHelp n xs $ sampleIntSubset n k

-- | Strict version of 'sampleSubset'.
sampleSubset' :: (PrimMonad m) => [a] -> Int -> MC m [a]
sampleSubset' xs k = do
    s <- sampleSubset xs k
    length s `seq` return s

-- | Sample a subset of the elements with the given weights.  Return
-- the elements of the subset lazily in the order they were sampled.
sampleSubsetWithWeights :: (PrimMonad m) => [(Double,a)] -> Int -> MC m [a]
sampleSubsetWithWeights wxs k = let
    (ws,xs) = unzip wxs
    n = length ws
    in sampleListHelp n xs $ sampleIntSubsetWithWeights ws n k

-- | Strict version of 'sampleSubsetWithWeights'.
sampleSubsetWithWeights' :: (PrimMonad m) => [(Double,a)] -> Int -> MC m [a]
sampleSubsetWithWeights' wxs k = do
    s <- sampleSubsetWithWeights wxs k
    length s `seq` return s

sampleHelp :: (PrimMonad m) => Int -> [a] -> MC m Int -> MC m a
sampleHelp _n xs f = let
    arr = BV.fromList xs
    in liftM (BV.unsafeIndex arr) f

sampleHelpU :: (Unbox a, Monad m) => Int -> [a] -> m Int -> m a
sampleHelpU _n xs f = let
    arr = V.fromList xs
    in liftM (V.unsafeIndex arr) f

{-# RULES "sampleHelp/Double" forall n xs f.
              sampleHelp n (xs :: [Double]) f = sampleHelpU n xs f #-}
{-# RULES "sampleHelp/Int" forall n xs f.
              sampleHelp n (xs :: [Int]) f = sampleHelpU n xs f #-}

sampleListHelp :: (Monad m) => Int -> [a] -> m [Int] -> m [a]
sampleListHelp _n xs f = let
    arr = BV.fromList xs
    in liftM (map $ BV.unsafeIndex arr) f

sampleListHelpU :: (Unbox a, Monad m) => Int -> [a] -> m [Int] -> m [a]
sampleListHelpU _n xs f = let
    arr = V.fromList xs
    in liftM (map $ V.unsafeIndex arr) f

{-# RULES "sampleListHelp/Double" forall n xs f.
              sampleListHelp n (xs :: [Double]) f = sampleListHelpU n xs f #-}
{-# RULES "sampleListHelp/Int" forall n xs f.
              sampleListHelp n (xs :: [Int]) f = sampleListHelpU n xs f #-}

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
-- were sampled.  Note also that the elements are lazily generated.
sampleIntSubset :: (PrimMonad m) => Int -> Int -> MC m [Int]
sampleIntSubset n k | k < 0     = fail "negative subset size"
                    | k > n     = fail "subset size is too big"
                    | otherwise = do
    us <- randomIndices n k
    return $ runST $ do
        ints <- MV.new n :: ST s (MVector s Int)
        sequence_ [ MV.unsafeWrite ints i i | i <- [0 .. n-1] ]
        sampleIntSubsetHelp ints us (n-1)
  where
    randomIndices n' k' | k' == 0   = return []
                        | otherwise = unsafeInterleaveMC $ do
        u  <- uniformInt n'
        us <- randomIndices (n'-1) (k'-1)
        return (u:us)

    sampleIntSubsetHelp :: MVector m Int -> [Int] -> Int -> ST m [Int]
    sampleIntSubsetHelp _    []     _  = return []
    sampleIntSubsetHelp ints (u:us) n' = unsafeInterleaveST $ do
        i <- MV.unsafeRead ints u
        MV.unsafeWrite ints u =<< MV.unsafeRead ints n'
        is <- sampleIntSubsetHelp ints us (n'-1)
        return (i:is)

-- | Strict version of 'sampleIntSubset'.
sampleIntSubset' :: (PrimMonad m) => Int -> Int -> MC m [Int]
sampleIntSubset' n k = do
    s <- sampleIntSubset n k
    length s `seq` return s

-- | @sampleIntSubsetWithWeights ws n k@ samplea size @k@ subset of
-- @{ 0, ..., n-1 }@ with the given weights by sampling elements without
-- replacement.  It returns the elements of the subset lazily in the order
-- they were sampled.
sampleIntSubsetWithWeights :: (PrimMonad m) => [Double] -> Int -> Int -> MC m [Int]
sampleIntSubsetWithWeights ws n k = let
    w_sum0 = foldl' (+) 0 $ take n ws
    wjs = [ (w / w_sum0, j) | (w,j) <- reverse $ sort $ zip ws [ 0..n-1 ] ]
    in do
        us <- replicateMC k $ uniform 0 1
        return $ runST $ do
            ints <- MV.new n :: ST s (MVector s (Double,Int))
            sequence_ [ MV.unsafeWrite ints i wj | (i,wj) <- zip [ 0.. ] wjs ]
            go ints n 1 us
  where
    go :: MVector m (Double, Int) -> Int -> Double -> [Double] -> ST m [Int]
    go ints n' w_sum us | null us   = return []
                        | otherwise = let
        target = head us * w_sum
        in unsafeInterleaveST $ do
            (i,(w,j)) <- findTarget ints n' target 0 0
            shiftDown ints (i+1) (n'-1)
            let w_sum' = w_sum - w
                n''    = n' - 1
                us'    = tail us
            js <- go ints n'' w_sum' us'
            return $ j:js

    findTarget :: MVector m (Double, Int)
                    -> Int -> Double -> Int -> Double -> ST m (Int, (Double, Int))
    findTarget ints n' target i acc
        | i == n' - 1 = do
            wj <- MV.unsafeRead ints i
            return (i,wj)
        | otherwise = do
            (w,j) <- MV.unsafeRead ints i
            let acc' = acc + w
            if target <= acc'
                then return (i,(w,j))
                else findTarget ints n' target (i+1) acc'

    shiftDown :: MVector m (Double, Int) -> Int -> Int -> ST m ()
    shiftDown ints from to =
        forM_ [ from..to ] $ \i -> do
            wj <- MV.unsafeRead ints i
            MV.unsafeWrite ints (i-1) wj


-- | Strict version of 'sampleIntSubsetWithWeights'.
sampleIntSubsetWithWeights' :: (PrimMonad m) => [Double] -> Int -> Int -> MC m [Int]
sampleIntSubsetWithWeights' ws n k = do
    s <- sampleIntSubsetWithWeights ws n k
    length s `seq` return s

-- | @shuffle xs@ randomly permutes the list @xs@ and returns
-- the result.  All permutations of the elements of @xs@ are equally
-- likely.
shuffle :: (PrimMonad m) => [a] -> MC m [a]
shuffle xs = let
    n = length xs
    in shuffleInt n >>= \swaps -> (return . BV.toList) $ BV.create $ do
           marr <- MV.new n
           zipWithM_ (MV.unsafeWrite marr) [0 .. n-1] xs
           mapM_ (swap marr) swaps
           return marr
  where
    swap :: BMV.MVector s a -> (Int,Int) -> ST s ()
    swap marr (i,j) | i == j    = return ()
                    | otherwise = do
        x <- MV.unsafeRead marr i
        y <- MV.unsafeRead marr j
        MV.unsafeWrite marr i y
        MV.unsafeWrite marr j x

shuffleU :: (Unbox a, PrimMonad m) => [a] -> MC m [a]
shuffleU xs = let
    n = length xs
    in shuffleInt n >>= \swaps -> (return . V.toList) $ V.create $ do
           marr <- MV.new n
           zipWithM_ (MV.unsafeWrite marr) [0 .. n-1] xs
           mapM_ (swap marr) swaps
           return marr
  where
    swap :: (Unbox a) => MVector s a -> (Int,Int) -> ST s ()
    swap marr (i,j) | i == j    = return ()
                    | otherwise = do
        x <- MV.unsafeRead marr i
        y <- MV.unsafeRead marr j
        MV.unsafeWrite marr i y
        MV.unsafeWrite marr j x

{-# RULES "shuffle/Double" forall xs.
              shuffle (xs :: [Double]) = shuffleU xs #-}
{-# RULES "shuffle/Int" forall xs.
              shuffle (xs :: [Int]) = shuffleU xs #-}


-- | @shuffleInt n@ generates a sequence of swaps equivalent to a
-- uniformly-chosen random permutatation of the integers @{0, ..., n-1}@.
-- For an input of @n@, there are @n-1@ swaps, which are lazily generated.
shuffleInt :: (PrimMonad m) => Int -> MC m [(Int,Int)]
shuffleInt n =
    let shuffleIntHelp i | i <= 1    = return []
                         | otherwise = unsafeInterleaveMC $ do
            j   <- uniformInt i
            ijs <- shuffleIntHelp (i-1)
            return $ (i-1,j):ijs in
    shuffleIntHelp n

-- | Strict version of 'shuffleInt'.
shuffleInt' :: (PrimMonad m) => Int -> MC m [(Int,Int)]
shuffleInt' n = do
    ss <- shuffleInt n
    length ss `seq` return ss
