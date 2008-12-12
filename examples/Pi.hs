
import Control.Monad.MC
import Control.Monad
import Data.List( foldl' )
import System.Environment( getArgs )
import Text.Printf( printf )

-- | Generate a point in the box [-1,1) x [-1,1)
unitBox :: MC (Double,Double)
unitBox = liftM2 (,) (uniform (-1) 1) 
                     (uniform (-1) 1)

-- | Indicates whether or not a point is in the unit circle
inUnitCircle :: (Double,Double) -> Bool
inUnitCircle (x,y) = x*x + y*y <= 1

-- | Given a list of indicators, return the sample mean and standard
-- error.
average :: [Bool] -> (Double,Double)
average is = let
    (t,n) = foldl' count (0,0) is
    p     = toDouble t / toDouble n
    se    = sqrt (p * (1 - p) / toDouble n)
    in (p, se)
  where
    count (t,n) i = let 
        t' = if i then t+1 else t
        n' = n+1
        in t' `seq` n' `seq` (t',n')

    toDouble = realToFrac . toInteger
        
-- | Compute a Monte Carlo estimate of pi based on @n@ samples.  Return
-- the estimate and the standard error of the estimate.
computePi :: Int -> MC (Double,Double)
computePi n = do
    is <- liftM (map inUnitCircle) (unsafeInterleaveMC $ replicateM n unitBox)
    let (mu ,se ) = average is
        (mu',se') = (4*mu,4*se)
    return (mu',se')

-- | Given an estimate and standard error, produce a 99% confidence
-- interval based on the Central Limit Theorem
interval :: Double -> Double -> (Double,Double)
interval mu se = let
    delta = 2.575*se
    in (mu-delta, mu+delta)

-- | Tests if the value is in the interval [a,b]
inInterval :: Double -> (Double,Double) -> Bool
x `inInterval` (a,b) = x >= a && x <= b

-- | Compute an estimate of pi based on @n@ points and see if the true
-- value is in the confidence interval
covers :: Int -> MC Bool
covers n = do
    (mu,se) <- computePi n
    return $ pi `inInterval` (interval mu se)

-- | Compute @r@ estimates of pi based on @n@ samples each, and count
-- how many times the true values is included in the 99% confidence
-- inverval
coverage :: Int -> Int -> MC Int
coverage r n = do
    liftM count $ replicateM r (covers n)
  where
    count = length . filter id
    
main = do
    [n] <- map read `fmap` getArgs
    main' n
    
main' n = let
    seed   = 0
    (mu,se) = evalMC (computePi n) $ mt19937 seed
    (l,u)   = interval mu se
    r       = 500
    c       = evalMC (coverage r n) $ mt19937 seed
    in do
        printf "Estimate from one simulation: %g\n" mu
        printf "99%% Confidence Interval:    (%g,%g)\n" l u
        printf "\nOf %d intervals, %d contain the true value.\n" r c
