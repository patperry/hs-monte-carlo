
In this example, we compute a Monte Carlo estimate of pi by
generating random points in the unit box, and counting how many
of them fall in the unit circle.

\begin{code}
import Control.Applicative( liftA2, pure )
import Control.Monad.MC( STMC, evalMC, foldMC, mt19937, uniform )
import Data.Monoid( (<>), mempty )
import Data.Summary.Bool( empty, update, sampleMean, sampleSE )
import Data.Summary( interval )
import Text.Printf( printf )
\end{code}

First, we need a function to test whether or not a point is in the
unit circle.  We define

\begin{code}
inUnitCircle :: (Double,Double) -> Bool
inUnitCircle (x,y) = x*x + y*y <= 1
\end{code}

Next, we need to generate a random point in the unit box.  In the
Control.Monad.MC module, there is a function for generating uniform
values in an interval, called "uniform".  This function has a
funny-looking type, but you can think of it as:

    uniform :: Double -> Double -> STMC s Double

This type means that the function takes the two endpoints of the
interval as arguments, and returns a Monte-Carlo action which produces
a Double.

You can think of the type "STMC s Double" as a random number generator.
For general types, "STMC s a" is a generator for values of type "a".

The following code generates an x value in the range (-1,1), then a y
value in the range (-1,1), then returns the pair (x,y):

\begin{code}
unitBox :: STMC s (Double,Double)
unitBox = liftA2 (,) (uniform (-1) 1)
                     (uniform (-1) 1)
\end{code}

-- | Compute a Monte Carlo estimate of pi based on @n@ samples.  Return
-- the sample mean and standard error.

\begin{code}
estimatePi :: Int -> STMC s (Double,Double)
estimatePi n = let
    circ = fmap inUnitCircle unitBox
    mc   = foldMC (\s x -> pure $! update s x) empty n circ
    in fmap (\s -> let (mu,se) = (sampleMean s, sampleSE s)
                   in (4*mu,4*se)) mc
\end{code}

\begin{code}
main =
    let seed    = 0
        n       = 1000000 
        (mu,se) = estimatePi n `evalMC` mt19937 seed
        (l,u)   = interval 0.95 mu se
    in do
        printf "\nEstimate: %g" mu
        printf "\n99%% Confidence Interval: (%g, %g)" l u
        printf "\n"
\end{code}
