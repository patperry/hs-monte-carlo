{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Control.Monad.MC.GSL
-- Copyright  : Copyright (c) , Patrick Perry <patperry@stanford.edu>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@stanford.edu>
-- Stability  : experimental
--

module Control.Monad.MC.GSL (
    -- * The Monte Carlo monad
    MC,
    runMC,
    evalMC,
    execMC,
    unsafeInterleaveMC,
    
    -- * The Monte Carlo monad transformer
    MCT,
    runMCT,
    evalMCT,
    execMCT,
    liftMCT,
    unsafeInterleaveMCT,

    -- | Pure random number generator creation
    RNG,
    mt19937,

    -- | Random distributions
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

-- | Get the baton from the Monte Carlo monad without performing any
-- computations.  Useful but dangerous.
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
    (MC m) >>= k =
        MC $ \r -> let
            (a, r') = m r
            (MC m') = k a
            in m' r'

instance MonadState RNG MC where
    get = MC $ getHelp 
    put r' = MC $ putHelp r' 

getHelp :: RNG -> (RNG,RNG)
getHelp r = unsafePerformIO $ do
    r' <- cloneRNG r
    r' `seq` return (r',r)
{-# NOINLINE getHelp #-}

putHelp :: RNG -> RNG -> ((),RNG)
putHelp r' r = unsafePerformIO $ do
    io <- copyRNG r r'
    io `seq` return ((),r)
{-# NOINLINE putHelp #-}

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

-- | Similar to 'unsafeInterleaveMC'.
unsafeInterleaveMCT :: (Monad m) => MCT m a -> MCT m a
unsafeInterleaveMCT (MCT g) = MCT $ \r -> do
    ~(a,_) <- g r
    return (a,r)

instance (Monad m) => Functor (MCT m) where
    fmap f (MCT m) = MCT $ \r -> do
        ~(x, r') <- m r
        return (f x, r')    

instance (Monad m) => Monad (MCT m) where
    return a = MCT $ \r -> return (a,r)
    
    (MCT m) >>= k =
        MCT $ \r -> do
            ~(a,r') <- m r
            let (MCT m') = k a
            m' r'
            
    fail str = MCT $ \_ -> fail str

instance (MonadPlus m) => MonadPlus (MCT m) where
    mzero = MCT $ \_ -> mzero
        
    (MCT m) `mplus` (MCT n) = 
        MCT $ \r ->
            let r' = unsafePerformIO $ cloneRNG r
            in r' `seq` (m r `mplus` n r')

instance (Monad m) => MonadState RNG (MCT m) where
    get    = MCT $ return . getHelp 
    put r' = MCT $ return . (putHelp r')

instance MonadTrans MCT where
    lift m = MCT $ \r -> do
        a <- m
        return (a,r)

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

-- | Get a Mersenne Twister random number generator seeded with the given
-- value.
mt19937 :: Word64 -> RNG
mt19937 s = unsafePerformIO $ do
    r <- newRNG Gen.mt19937
    setSeed r s
    return r
{-# NOINLINE mt19937 #-}

-- | @uniformInt n@ generates an integer uniformly in the range @[0,n-1]@.
-- It is an error to call this function with a non-positive value.
uniformInt :: Int -> MC Int
uniformInt n = MC $ uniformIntHelp n

uniformIntHelp :: Int -> RNG -> (Int,RNG)
uniformIntHelp n r = unsafePerformIO $ do
    x <- getUniformInt r n
    x `seq` return (x,r)

-- | @uniform  a b@ generates a value uniformly distributed in @[a,b)@.
uniform :: Double -> Double -> MC Double
uniform a b = MC $ uniformHelp a b

uniformHelp :: Double -> Double -> RNG -> (Double,RNG)
uniformHelp a b r = unsafePerformIO $ do
    x <- getFlat r a b
    x `seq` return (x,r)
{-# NOINLINE uniformHelp #-}
    
-- | @normal mu sigma@ generates a Normal random variable with mean
-- @mu@ and standard deviation @sigma@.
normal :: Double -> Double -> MC Double
normal mu sigma = MC $ normalHelp mu sigma

normalHelp :: Double -> Double -> RNG -> (Double,RNG)
normalHelp mu sigma r = unsafePerformIO $ do
    x <- liftM (mu +) $ getGaussian r sigma
    x `seq` return (x,r)
{-# NOINLINE normalHelp #-}

-- | @poisson mu@ generates a Poisson random variable with mean @mu@.
poisson :: Double -> MC Int
poisson mu = MC $ poissonHelp mu

poissonHelp :: Double -> RNG -> (Int,RNG)
poissonHelp mu r = unsafePerformIO $ do
    x <- getPoisson r mu
    x `seq` return (x,r)
{-# NOINLINE poissonHelp #-}


{-


unifInt :: (Monad m) => Int -> MCT m Int
unifInt n = MCT $ unifInt' n

unifInt' :: (Monad m) => Int -> RNG -> m (Int,RNG)
unifInt' n r =
    unsafePerformIO $ do
        i <- rngUnifInt r n
        i `seq` (return . return) (i,r)
-}