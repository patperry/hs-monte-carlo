{-# LANGUAGE DeriveDataTypeable, RankNTypes, TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module     : Control.Monad.MC.GSLBase
-- Copyright  : Copyright (c) 2010, Patrick Perry <patperry@gmail.com>
-- License    : BSD3
-- Maintainer : Patrick Perry <patperry@gmail.com>
-- Stability  : experimental
--

module Control.Monad.MC.GSLBase (
    -- * Monte Carlo monad transformer
    MC(..),
    STMC,
    IOMC,
    evalMC,

    -- * Random number generator
    -- ** Types
    RNG,
    IORNG,
    STRNG,
    Seed,
    -- ** Creation
    mt19937,
    mt19937WithState,
    -- ** State
    getRNGName,
    getRNGSize,
    getRNGState,
    setRNGState,

    -- * Random number distributions
    -- ** Uniform
    uniform,
    uniformInt,
    -- ** Continuous
    normal,
    exponential,
    gamma,
    cauchy,
    levy,
    levySkew,
    pareto,
    weibull,
    logistic,
    beta,
    -- ** Discrete
    bernoulli,
    poisson,
    -- ** Multivariate
    dirichlet,
    multinomial,
    ) where

import Control.Applicative       ( Applicative(..) )
import Control.Monad             ( liftM )
import Control.Monad.Fix         ( MonadFix(..) )
import Control.Monad.IO.Class    ( MonadIO(..) )
import Control.Monad.ST          ( ST, runST )
import Control.Monad.Primitive   ( PrimMonad(..), unsafePrimToPrim )
import Control.Monad.Trans.Class ( MonadTrans(..) )
import Data.Typeable             ( Typeable )
import Data.Word                 ( Word8, Word64 )

import qualified Data.Vector.Storable as VS

import qualified GSL.Random.Gen as GSL
import GSL.Random.Dist


-- | A Monte Carlo monad transformer.  This type provides access
-- to a random number generator while allowing operations in a
-- base monad, @m@.
newtype MC m a = MC { runMC :: RNG (PrimState m) -> m a }


-- | Type alias for when the base monad is 'ST'.
type STMC s a = MC (ST s) a

-- | Type alias for when the base monad is 'IO'.
type IOMC a = MC IO a


-- | Evaluate the result of a Monte Carlo computation using the given
-- random number generator.
evalMC :: (forall s. STMC s a) -> (forall s. ST s (STRNG s)) -> a
evalMC ma mr = runST $ do
    r <- mr
    runMC ma r


instance (Functor m) => Functor (MC m) where
    fmap f (MC m) = MC $ \r -> fmap f (m r)
    {-# INLINE fmap #-}

instance (Applicative m) => Applicative (MC m) where
    pure a = MC $ \_ -> pure a
    {-# INLINE pure #-}

    (MC gf) <*> (MC ga) = MC $ \r -> (gf r) <*> (ga r)
    {-# INLINE (<*>) #-}

instance (Monad m) => Monad (MC m) where
    return a = MC $ \_ -> return a
    {-# INLINE return #-}

    (MC m) >>= k =
        MC $ \r -> m r >>= \a ->
            let (MC m') = k a
            in m' r
    {-# INLINE (>>=) #-}

    fail msg = MC $ \_ -> fail msg
    {-# INLINE fail #-}

instance (MonadFix m) => MonadFix (MC m) where
    mfix f = MC $ \r -> mfix $ flip (runMC . f) r
    {-# INLINE mfix #-}

instance (MonadIO m) => MonadIO (MC m) where
    liftIO io = MC $ \_ -> liftIO io
    {-# INLINE liftIO #-}

instance MonadTrans MC where
    lift m = MC $ \_ -> m
    {-# INLINE lift #-}


---------------------------- Random Number Generators -----------------------

-- | The random number generator type.
newtype RNG s = RNG GSL.RNG
    deriving(Eq, Show, Typeable)

-- | A shorter name for RNG in the 'IO' monad.
type IORNG = RNG (PrimState IO)

-- | A shorter name for RNG in the 'ST' monad.
type STRNG s = RNG (PrimState (ST s))

-- | The seed type for the random number generators.
type Seed = Word64


-- | Get the name of the random number generator algorithm.
getRNGName :: (PrimMonad m) => RNG (PrimState m) -> m String
getRNGName (RNG r) = unsafePrimToPrim $ GSL.getName r

-- | Get the size of the generator state, in bytes.
getRNGSize :: (PrimMonad m) => RNG (PrimState m) -> m Int
getRNGSize (RNG r) = liftM fromIntegral $ unsafePrimToPrim $ GSL.getSize r

-- | Get the state of the generator.
getRNGState :: (PrimMonad m) => RNG (PrimState m) -> m [Word8]
getRNGState (RNG r) = unsafePrimToPrim $ GSL.getState r

-- | Set the state of the generator.
setRNGState :: (PrimMonad m) => RNG (PrimState m) -> [Word8] -> m ()
setRNGState (RNG r) xs = unsafePrimToPrim $ GSL.setState r xs

-- | Create a Mersenne Twister random number generator seeded with the given
-- value.
mt19937 :: (PrimMonad m) => Seed -> m (RNG (PrimState m))
mt19937 s = unsafePrimToPrim $ do
    r <- GSL.newRNG GSL.mt19937
    GSL.setSeed r s
    return (RNG r)

-- | Create a Mersenne Twister seeded with the given state.
mt19937WithState :: (PrimMonad m) => [Word8] -> m (RNG (PrimState m))
mt19937WithState xs = unsafePrimToPrim $ do
    r <- GSL.newRNG GSL.mt19937
    GSL.setState r xs
    return (RNG r)

-------------------------- Random Number Distributions ----------------------

-- | @uniform a b@ generates a value uniformly distributed in @[a,b)@.
uniform :: (PrimMonad m) => Double -> Double -> MC m Double
uniform 0 1 = liftRan0 GSL.getUniform
uniform a b = liftRan2 getFlat a b

-- | @uniformInt n@ generates an integer uniformly in the range @[0,n-1]@.
-- It is an error to call this function with a non-positive value.
uniformInt :: (PrimMonad m) => Int -> MC m Int
uniformInt = liftRan1 GSL.getUniformInt

-- | @normal mu sigma@ generates a Normal random variable with mean
-- @mu@ and standard deviation @sigma@.
normal :: (PrimMonad m) => Double -> Double -> MC m Double
normal 0  1     =                liftRan0 getUGaussianRatioMethod
normal mu 1     = liftM (mu +) $ liftRan0 getUGaussianRatioMethod
normal 0  sigma =                liftRan1 getGaussianRatioMethod sigma
normal mu sigma = liftM (mu +) $ liftRan1 getGaussianRatioMethod sigma

-- | @exponential mu@ generates an Exponential variate with mean @mu@.
exponential :: (PrimMonad m) => Double -> MC m Double
exponential = liftRan1 getExponential

-- | @poisson mu@ generates a Poisson random variable with mean @mu@.
poisson :: (PrimMonad m) => Double -> MC m Int
poisson = liftRan1 getPoisson

-- | @levy c alpha@ gets a Levy alpha-stable variate with scale @c@ and
-- exponent @alpha@.  The algorithm only works for @0 < alpha <= 2@.
levy :: (PrimMonad m) => Double -> Double -> MC m Double
levy = liftRan2 getLevy

-- | @levySkew c alpha beta @ gets a skew Levy alpha-stable variate
-- with scale @c@, exponent @alpha@, and skewness @beta@.  The skew
-- parameter must lie in the range @[-1,1]@.  The algorithm only works
-- for @0 < alpha <= 2@.
levySkew :: (PrimMonad m) => Double -> Double -> Double -> MC m Double
levySkew = liftRan3 getLevySkew

-- | @cauchy a@ generates a Cauchy random variable with scale
-- parameter @a@.
cauchy :: (PrimMonad m) => Double -> MC m Double
cauchy = liftRan1 getCauchy

-- | @beta a b@ generates a beta random variable with
-- parameters @a@ and @b@.
beta :: (PrimMonad m) => Double -> Double -> MC m Double
beta = liftRan2 getBeta

-- | @logistic a@ generates a logistic random variable with
-- parameter @a@.
logistic :: (PrimMonad m) => Double -> MC m Double
logistic = liftRan1 getLogistic

-- | @pareto a b@ generates a Pareto random variable with
-- exponent @a@ and scale @b@.
pareto :: (PrimMonad m) => Double -> Double -> MC m Double
pareto = liftRan2 getPareto

-- | @weibull a b@ generates a Weibull random variable with
-- scale @a@ and exponent @b@.
weibull :: (PrimMonad m) => Double -> Double -> MC m Double
weibull = liftRan2 getWeibull

-- | @gamma a b@ generates a gamma random variable with
-- parameters @a@ and @b@.
gamma :: (PrimMonad m) => Double -> Double -> MC m Double
gamma = liftRan2 getGamma

-- | @multinomial n ps@ generates a multinomial random
-- variable with parameters @ps@ formed by @n@ trials.
multinomial :: (PrimMonad m) => Int -> VS.Vector Double -> MC m (VS.Vector Int)
multinomial = liftRan2 getMultinomial

-- | @dirichlet alphas@ generates a Dirichlet random variable
-- with parameters @alphas@.
dirichlet :: (PrimMonad m) => VS.Vector Double -> MC m (VS.Vector Double)
dirichlet = liftRan1 getDirichlet

-- | Generate 'True' events with the given probability.
bernoulli :: (PrimMonad m ) => Double -> MC m Bool
bernoulli p = liftM (< p) $ uniform 0 1
{-# INLINE bernoulli #-}


liftRan0 :: (PrimMonad m) => (GSL.RNG -> IO a) -> MC m a
liftRan0 f = MC $ \(RNG r) -> unsafePrimToPrim $ f r

liftRan1 :: (PrimMonad m) => (GSL.RNG -> a -> IO b) -> a -> MC m b
liftRan1 f a = MC $ \(RNG r) -> unsafePrimToPrim $ f r a

liftRan2 :: (PrimMonad m) => (GSL.RNG -> a -> b -> IO c) -> a -> b -> MC m c
liftRan2 f a b = MC $ \(RNG r) -> unsafePrimToPrim $ f r a b

liftRan3 :: (PrimMonad m) => (GSL.RNG -> a -> b -> c -> IO d) -> a -> b -> c -> MC m d
liftRan3 f a b c = MC $ \(RNG r) -> unsafePrimToPrim $ f r a b c
