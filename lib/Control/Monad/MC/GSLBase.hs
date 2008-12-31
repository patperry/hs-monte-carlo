{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Control.Monad.MC.GSLBase
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

module Control.Monad.MC.GSLBase (
    -- * The Monte Carlo monad
    MC(..),
    runMC,
    evalMC,
    execMC,
    unsafeInterleaveMC,
    
    -- * The Monte Carlo monad transformer
    MCT(..),
    runMCT,
    evalMCT,
    execMCT,
    unsafeInterleaveMCT,
    liftMCT,

    -- * Pure random number generator creation
    RNG,
    mt19937,

    -- * Getting and setting the random number generator
    getRNG,
    setRNG,

    -- * Random number distributions
    uniform,
    uniformInt,
    normal,
    poisson,
    ) where

import Control.Monad            ( liftM, MonadPlus(..) )
import Control.Monad.Cont       ( MonadCont(..) )
import Control.Monad.Error      ( MonadError(..) )
import Control.Monad.Reader     ( MonadReader(..) )
import Control.Monad.State      ( MonadState(..) )
import Control.Monad.Writer     ( MonadWriter(..) )
import Control.Monad.Trans      ( MonadTrans(..), MonadIO(..) )
import Data.Word
import System.IO.Unsafe         ( unsafePerformIO )
        
import GSL.Random.Gen hiding ( mt19937 )
import qualified GSL.Random.Gen as Gen
import GSL.Random.Dist

-- | A Monte Carlo monad with an internal random number generator.
newtype MC a = MC (RNG -> (a,RNG))

-- | Run this Monte Carlo monad with the given initial random number generator,
-- getting the result and the new random number generator.
runMC :: MC a -> RNG -> (a, RNG)
runMC (MC g) r =
    let r' = unsafePerformIO $ cloneRNG r
    in r' `seq` g r'
{-# NOINLINE runMC #-}
    
-- | Evaluate this Monte Carlo monad and throw away the final random number
-- generator.  Very much like @fst@ composed with @runMC@.
evalMC :: MC a -> RNG -> a
evalMC g r = fst $ runMC g r

-- | Exicute this Monte Carlo monad and return the final random number
-- generator.  Very much like @snd@ composed with @runMC@.
execMC :: MC a -> RNG -> RNG
execMC g r = snd $ runMC g r

unsafeInterleaveMC :: MC a -> MC a
unsafeInterleaveMC (MC m) = MC $ \r -> let
    (a,_) = m r
    in (a,r)


instance Functor MC where
    fmap f (MC m) = MC $ \r -> let
        (a,r') = m r
        in (f a, r')

instance Monad MC where
    return a = MC $ \r -> (a,r)
    {-# INLINE return #-}
    
    (MC m) >>= k =
        MC $ \r -> let
            (a, r') = m r
            (MC m') = k a
            in m' r'
    {-# INLINE (>>=) #-}

-- | A parameterizable Monte Carlo monad for encapsulating an inner
-- monad.
newtype MCT m a = MCT (RNG -> m (a,RNG))

-- | Similar to 'runMC'.
runMCT :: (Monad m) => MCT m a -> RNG -> m (a,RNG)
runMCT (MCT g) r =
    let r' = unsafePerformIO $ cloneRNG r
    in r' `seq` g r'
{-# NOINLINE runMCT #-}

-- | Similar to 'evalMC'.
evalMCT :: (Monad m) => MCT m a -> RNG -> m a
evalMCT g r = do
    ~(a,_) <- runMCT g r
    return a
    
-- | Similar to 'execMC'.    
execMCT :: (Monad m) => MCT m a -> RNG -> m RNG
execMCT g r = do
    ~(_,r') <- runMCT g r
    return r'

-- | Take a Monte Carlo computations and lift it to an MCT computation.
liftMCT :: (Monad m) => MC a -> MCT m a
liftMCT (MC m) = MCT $ return . m
{-# INLINE liftMCT #-}

unsafeInterleaveMCT :: (Monad m) => MCT m a -> MCT m a
unsafeInterleaveMCT (MCT g) = MCT $ \r -> do
    ~(a,_) <- g r
    return (a,r)
{-# INLINE unsafeInterleaveMCT #-}

instance (Monad m) => Functor (MCT m) where
    fmap f (MCT m) = MCT $ \r -> do
        ~(x, r') <- m r
        return (f x, r') 
    {-# INLINE fmap #-}   

instance (Monad m) => Monad (MCT m) where
    return a = MCT $ \r -> return (a,r)
    {-# INLINE return #-}
    
    (MCT m) >>= k =
        MCT $ \r -> do
            ~(a,r') <- m r
            let (MCT m') = k a
            m' r'
    {-# INLINE (>>=) #-}
            
    fail str = MCT $ \_ -> fail str

instance (MonadPlus m) => MonadPlus (MCT m) where
    mzero = MCT $ \_ -> mzero
    {-# INLINE mzero #-}
        
    (MCT m) `mplus` (MCT n) = 
        MCT $ \r ->
            let r' = unsafePerformIO $ cloneRNG r
            in r' `seq` (m r `mplus` n r')
    {-# NOINLINE mplus #-}

instance MonadTrans MCT where
    lift m = MCT $ \r -> do
        a <- m
        return (a,r)
    {-# INLINE lift #-}

instance (MonadCont m) => MonadCont (MCT m) where
    callCC f = MCT $ \r ->
        callCC $ \c ->
        let (MCT m) = (f (\a -> MCT $ \r' -> c (a, r'))) 
        in m r

instance (MonadError e m) => MonadError e (MCT m) where
    throwError              = lift . throwError
    (MCT m) `catchError` h = MCT $ \r -> 
        m r `catchError` \e -> let (MCT m') = h e in m' r

instance (MonadIO m) => MonadIO (MCT m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance (MonadReader r m) => MonadReader r (MCT m) where
    ask              = lift ask
    local f (MCT m) = MCT $ \r ->
        local f (m r)

instance (MonadState s m) => MonadState s (MCT m) where
    get = lift get 
    put = lift . put

instance (MonadWriter w m) => MonadWriter w (MCT m) where
    tell            = lift . tell
    listen (MCT m) = MCT $ \r -> do
        ~((a,r'),w) <- listen (m r)
        return ((a,w),r')
    pass (MCT m) = MCT $ \r -> pass $ do
        ~((a,f),r') <- m r
        return ((a,r'),f)

---------------------------- Random Number Generators -----------------------

getRNG :: MC RNG
getRNG = MC $ getHelp 
{-# INLINE getRNG #-}

getHelp :: RNG -> (RNG,RNG)
getHelp r = unsafePerformIO $ do
    r' <- cloneRNG r
    r' `seq` return (r',r)
{-# NOINLINE getHelp #-}

setRNG :: RNG -> MC ()
setRNG r' = MC $ setHelp r'
{-# INLINE setRNG #-}

setHelp :: RNG -> RNG -> ((),RNG)
setHelp r' r = unsafePerformIO $ do
    io <- copyRNG r r'
    io `seq` return ((),r)
{-# NOINLINE setHelp #-}

-- | Get a Mersenne Twister random number generator seeded with the given
-- value.
mt19937 :: Word64 -> RNG
mt19937 s = unsafePerformIO $ do
    r <- newRNG Gen.mt19937
    setSeed r s
    return r
{-# NOINLINE mt19937 #-}


-------------------------- Random Number Distributions ----------------------

uniform :: Double -> Double -> MC Double
uniform a b = MC $ uniformHelp a b
{-# INLINE uniform #-}

uniformHelp :: Double -> Double -> RNG -> (Double,RNG)
uniformHelp 0 1 r = unsafePerformIO $ do
    x <- getUniform r
    x `seq` return (x,r)
uniformHelp a b r = unsafePerformIO $ do
    x <- getFlat r a b
    x `seq` return (x,r)
{-# NOINLINE uniformHelp #-}
    
uniformInt :: Int -> MC Int
uniformInt n = MC $ uniformIntHelp n
{-# INLINE uniformInt #-}

uniformIntHelp :: Int -> RNG -> (Int,RNG)
uniformIntHelp n r = unsafePerformIO $ do
    x <- getUniformInt r n
    x `seq` return (x,r)
{-# NOINLINE uniformIntHelp #-}

normal :: Double -> Double -> MC Double
normal mu sigma = MC $ normalHelp mu sigma
{-# INLINE normal #-}

normalHelp :: Double -> Double -> RNG -> (Double,RNG)
normalHelp 0 1 r = unsafePerformIO $ do
    x <- getUGaussianRatioMethod r
    x `seq` return (x,r)
normalHelp mu 1 r = unsafePerformIO $ do
    x <- liftM (mu +) $ getUGaussianRatioMethod r
    x `seq` return (x,r)
normalHelp 0 sigma r = unsafePerformIO $ do
    x <- getGaussianRatioMethod r sigma
    x `seq` return (x,r)
normalHelp mu sigma r = unsafePerformIO $ do
    x <- liftM (mu +) $ getGaussianRatioMethod r sigma
    x `seq` return (x,r)
{-# NOINLINE normalHelp #-}

poisson :: Double -> MC Int
poisson mu = MC $ poissonHelp mu
{-# INLINE poisson #-}

poissonHelp :: Double -> RNG -> (Int,RNG)
poissonHelp mu r = unsafePerformIO $ do
    x <- getPoisson r mu
    x `seq` return (x,r)
{-# NOINLINE poissonHelp #-}
