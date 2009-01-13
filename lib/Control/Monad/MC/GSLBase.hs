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
    Seed,
    mt19937,
    mt19937WithState,
    rngName,
    rngSize,
    rngState,

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
import System.IO.Unsafe         ( unsafePerformIO, unsafeInterleaveIO )
        
import qualified GSL.Random.Gen as GSL
import GSL.Random.Dist

-- | A Monte Carlo monad with an internal random number generator.
newtype MC a = MC (GSL.RNG -> IO a)

-- | Run this Monte Carlo monad with the given initial random number generator,
-- getting the result and the new random number generator.
runMC :: MC a -> RNG -> (a, RNG)
runMC (MC g) (RNG r) = unsafePerformIO $ do
    r' <- GSL.cloneRNG r
    a  <- g r'
    return (a,RNG r')
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
unsafeInterleaveMC (MC m) = MC $ \r ->
    unsafeInterleaveIO (m r)

instance Functor MC where
    fmap f (MC m) = MC $ \r ->
        fmap f (m r)

instance Monad MC where
    return a = MC $ \_ -> return a
    {-# INLINE return #-}
    
    (MC m) >>= k =
        MC $ \r -> m r >>= \a ->
            let (MC m') = k a
            in m' r
    {-# INLINE (>>=) #-}
    
    fail s = MC $ \_ -> fail s
    {-# INLINE fail #-}

-- | A parameterizable Monte Carlo monad for encapsulating an inner
-- monad.
newtype MCT m a = MCT (GSL.RNG -> IO (m a))

-- | Similar to 'runMC'.
runMCT :: (Monad m) => MCT m a -> RNG -> m (a,RNG)
runMCT (MCT g) (RNG r) = unsafePerformIO $ do
    r' <- GSL.cloneRNG r
    ma <- g r' 
    return (ma >>= \a -> return (a, RNG r'))
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
liftMCT (MC g) = MCT $ \r -> do
    a <- g r
    return (return a)
{-# INLINE liftMCT #-}

unsafeInterleaveMCT :: (Monad m) => MCT m a -> MCT m a
unsafeInterleaveMCT (MCT g) = MCT $ \r -> 
    unsafeInterleaveIO (g r)
{-# INLINE unsafeInterleaveMCT #-}

instance (Monad m) => Functor (MCT m) where
    fmap f (MCT g) = MCT $ \r -> do
        ma <- g r
        return (ma >>= return . f)
    {-# INLINE fmap #-}   

instance (Monad m) => Monad (MCT m) where
    return a = MCT $ \_ -> return (return a)
    {-# INLINE return #-}
    
    (MCT g) >>= k =
        MCT $ \r -> do
            ma <- g r
            return $ ma >>= \a ->
                let (MCT m') = k a
                in unsafePerformIO $ m' r
    {-# NOINLINE (>>=) #-}
            
    fail str = MCT $ \_ -> fail str
    {-# INLINE fail #-}

instance (MonadPlus m) => MonadPlus (MCT m) where
    mzero = MCT $ \_ -> mzero
    {-# INLINE mzero #-}
        
    (MCT m) `mplus` (MCT n) = 
        MCT $ \r -> do
            r' <- GSL.cloneRNG r
            mr <- m r
            nr <- n r'
            return (mr `mplus` nr)

instance MonadTrans MCT where
    lift m = MCT $ \_ -> return m
    {-# INLINE lift #-}

instance (MonadCont m) => MonadCont (MCT m) where
    callCC f = MCT $ \r ->
        return $ callCC $ \k ->
            let (MCT m) = f (\a -> MCT $ \_ -> return (k a))
            in unsafePerformIO (m r)
    {-# NOINLINE callCC #-}

instance (MonadError e m) => MonadError e (MCT m) where
    throwError             = lift . throwError
    {-# INLINE throwError #-}
    
    (MCT g) `catchError` h = MCT $ \r -> do
        ma <- g r
        return $ ma `catchError` \e -> 
            let (MCT m') = h e 
            in unsafePerformIO (m' r)
    {-# NOINLINE catchError #-}

instance (MonadIO m) => MonadIO (MCT m) where
    liftIO = lift . liftIO
    {-# INLINE liftIO #-}

instance (MonadReader r m) => MonadReader r (MCT m) where
    ask              = lift ask
    {-# INLINE ask #-}
    
    local f (MCT g) = MCT $ \r -> do
        ma <- g r
        return $ local f ma
    {-# INLINE local #-}

instance (MonadState s m) => MonadState s (MCT m) where
    get = lift get 
    {-# INLINE get #-}
    
    put = lift . put
    {-# INLINE put #-}

instance (MonadWriter w m) => MonadWriter w (MCT m) where
    tell           = lift . tell
    {-# INLINE tell #-}
    
    listen (MCT g) = MCT $ \r -> do
        ma <- g r
        return (listen ma)
    {-# INLINE listen #-}
    
    pass (MCT g) = MCT $ \r -> do
        maf <- g r
        return (pass maf)
    {-# INLINE pass #-}

---------------------------- Random Number Generators -----------------------

-- | The random number generator type associated with 'MC' and 'MCT'.
newtype RNG = RNG GSL.RNG

-- | The seed type for the random number generators.
type Seed = Word64

-- | Get the name of the random number generator algorithm.
rngName :: RNG -> String
rngName (RNG r) = unsafePerformIO $ GSL.getName r
{-# NOINLINE rngName #-}

-- | Get the size of the generator state, in bytes.
rngSize :: RNG -> Int
rngSize (RNG r) = fromIntegral $ unsafePerformIO $ GSL.getSize r
{-# NOINLINE rngSize #-}

-- | Get the state of the generator.
rngState :: RNG -> [Word8]
rngState (RNG r) = unsafePerformIO $ GSL.getState r
{-# NOINLINE rngState #-}

getRNG :: MC RNG
getRNG = MC (\r -> liftM RNG $ GSL.cloneRNG r)
{-# INLINE getRNG #-}

setRNG :: RNG -> MC ()
setRNG (RNG r') = MC $ \r -> GSL.copyRNG r r'
{-# INLINE setRNG #-}

-- | Get a Mersenne Twister random number generator seeded with the given
-- value.
mt19937 :: Word64 -> RNG
mt19937 s = unsafePerformIO $ do
    r <- GSL.newRNG GSL.mt19937
    GSL.setSeed r s
    return (RNG r)
{-# NOINLINE mt19937 #-}

-- | Get a Mersenne Twister seeded with the given state.
mt19937WithState :: [Word8] -> RNG
mt19937WithState xs = unsafePerformIO $ do
    r <- GSL.newRNG GSL.mt19937
    GSL.setState r xs
    return (RNG r)
{-# NOINLINE mt19937WithState #-}

-------------------------- Random Number Distributions ----------------------

uniform :: Double -> Double -> MC Double
uniform 0 1 = MC $ \r -> GSL.getUniform r
uniform a b = MC $ \r -> getFlat r a b
    
uniformInt :: Int -> MC Int
uniformInt n = MC $ \r -> GSL.getUniformInt r n

normal :: Double -> Double -> MC Double
normal 0  1     = MC $ \r -> getUGaussianRatioMethod r
normal mu 1     = MC $ \r -> liftM (mu +) (getUGaussianRatioMethod r)
normal 0  sigma = MC $ \r -> getGaussianRatioMethod r sigma
normal mu sigma = MC $ \r -> liftM (mu +) (getGaussianRatioMethod r sigma)

poisson :: Double -> MC Int
poisson mu = MC $ \r -> getPoisson r mu
