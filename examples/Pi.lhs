
In this example, we compute a Monte Carlo estimate of pi by
generating random points in the unit box, and counting how many
of them fall in the unit circle.

\begin{code}
import Control.Monad( liftM, liftM2 )
import Control.Monad.MC( MC, uniform, replicateMC, evalMC, mt19937 )
import Data.Summary.Bool( summary, sampleMean, sampleSE, interval )
import Text.Printf
\end{code}

First, we need a function to test whether or not a point is in the
unit circle.  We define

\begin{code}
inUnitCircle :: (Double,Double) -> Bool
inUnitCircle (x,y) = x*x + y*y <= 1
\end{code}

The first line is the type signature, which tells us that "inUnitCircle"
is a function which takes a pair of `Double`s and returns a `Bool`.  In
English, "::" means "has type".  The second line is the one that defines
the function.

\begin{code}
estimatePi :: [(Double,Double)] -> (Double,Double)
estimatePi xs =
    let s       = summary $ map inUnitCircle xs
        (mu,se) = (sampleMean s, sampleSE s) in
    (4*mu,4*se)
\end{code}

Next, we need to generate a random point in the unit box.  In the
Control.Monad.MC module, there is a function for generating uniform
values in an interval, called "uniform".  This function has a 
funny-looking type, but you can think of it as:

    uniform :: Double -> Double -> MC Double

This type means that the function takes the two endpoints of the 
interval as arguments, and returns a Monte-Carlo action which produces
a Double.

You can think of the type "MC Double" as a random number generator.
For general types, "MC a" is a generator for values of type "a".  In
fact, "MC" is one of a general class of objects called a Monad.  We
use Monads in Haskell for making sure events happen in the right order.
That is, if we have three parts of a simulation, say A, B, and C, and
we want them to happen in the order
 
    A ====> B ====> C

then we would like to make sure that A is done consuming random numbers
before B consumes anything.  Likewise, we want B to finish before C
starts.  Monads are the magic that enable us to ensure this.

There are a number of functions in the standard library for working with
monads.  The first we will use is

    liftM2 :: (Monad m) => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r

This function works on *any* Monad.  When we use it on the MC monad, it
will have type

    liftM2 :: (a1 -> a2 -> r) -> MC a1 -> MC a2 -> MC r
    
What liftM2 does is it takes a function of two arguments and two Monte
Carlo actions.  It returns a new Monte Carlo action that does the following:

  1. generate a random value of type a1 using the first action
  2. generate a random value of type a2 using the second action
  3. apply a function to the two values and return the result
  
We do not need to write liftM2 ourselves, since it is provided in the 
"Control.Monad" module.  But, if we did have to define it, the code would
look like:

liftM2 f ma1 ma2 = do
    a1 <- ma1
    a2 <- ma2
    return (f a1 a2)
    
This code for this uses the "do" notation of Haskell, which allows us
to specify a series of actions in sequential order.

\begin{code}
unitBox :: MC (Double,Double)
unitBox = liftM2 (,) (uniform (-1) 1) 
                     (uniform (-1) 1)
\end{code}

-- | Compute a Monte Carlo estimate of pi based on @n@ samples.  Return
-- the sample mean and standard error.

\begin{code}
simulation :: Int -> MC (Double,Double)
simulation n = 
    estimatePi `fmap` replicateMC n unitBox
\end{code}

\begin{code}
main =
    let seed    = 0
        n       = 1000000 
        (mu,se) = simulation n `evalMC` mt19937 seed
        (l,u)   = interval 0.95 mu se
    in do
        printf "\nEstimate: %g" mu
        printf "\n99%% Confidence Interval: (%g, %g)" l u
        printf "\n"
\end{code}
