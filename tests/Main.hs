
module Main where

import Debug.Trace
import Control.Monad
import Data.AEq
import Data.List
import System.IO
import System.Environment
import System.Random
import Text.Printf
import Test.QuickCheck

import Control.Monad.MC.Walker


prop_table_probs (Weights n ws) =
    let table = computeTable n ws
    in all (\i -> probOf table i ~== ps !! i) [0..n-1]
  where
    ps = probsFromWeights ws

prop_table_index (Weights n ws) (Unif u) =
    let table = computeTable n ws
        i     = indexTable table u
    in i >= 0 && i < n && (ws !! i > 0)

tests_Walker = [ ("table probabilities", mytest prop_table_probs)
               , ("table indexing"     , mytest prop_table_index)
               ]

probOf table i =
    (((sum . map ((1-) . fst) . filter ((==i) . snd))
                       (map (component table) [0..n-1]))
                       + (fst . component table) i) / fromIntegral n
  where
    n = tableSize table

------------------------------- Utility functions ---------------------------

probsFromWeights ws = let
    w  = sum ws
    ps = map (/w) ws
    in ps

------------------------------- Test generators -----------------------------

posInt :: Gen Int
posInt = do
    n <- arbitrary
    return $! abs n + 1

weight :: Gen Double
weight = do
    w <- liftM abs arbitrary
    if w < infty then return w else weight
  where
    infty = 1/0

weights :: Int -> Gen [Double]
weights n = do
    ws <- replicateM n weight
    if not (all (== 0) ws) then return ws else return $ replicate n 1.0

unif :: Gen Double
unif = do
    u <- choose (0,1)
    if u == 1 then return 0 else return u

data Weights = Weights Int [Double] deriving Show
instance Arbitrary Weights where
    arbitrary = do
        n  <- posInt
        ws <- weights n
        return $ Weights n ws

    coarbitrary (Weights n ws) =
        coarbitrary (n,ws)

data Unif = Unif Double deriving Show
instance Arbitrary Unif where
    arbitrary            = liftM Unif unif
    coarbitrary (Unif u) = coarbitrary u

------------------------------------------------------------------------
--
-- QC driver ( taken from xmonad-0.6 )
--

debug = False

mytest :: Testable a => a -> Int -> IO (Bool, Int)
mytest a n = mycheck defaultConfig
    { configMaxTest=n
    , configEvery   = \n args -> let s = show n in s ++ [ '\b' | _ <- s ] } a
 -- , configEvery= \n args -> if debug then show n ++ ":\n" ++ unlines args else [] } a

mycheck :: Testable a => Config -> a -> IO (Bool, Int)
mycheck config a = do
    rnd <- newStdGen
    mytests config (evaluate a) rnd 0 0 []

mytests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO (Bool, Int)
mytests config gen rnd0 ntest nfail stamps
    | ntest == configMaxTest config = done "OK," ntest stamps >> return (True, ntest)
    | nfail == configMaxFail config = done "Arguments exhausted after" ntest stamps >> return (True, ntest)
    | otherwise               =
      do putStr (configEvery config ntest (arguments result)) >> hFlush stdout
         case ok result of
           Nothing    ->
             mytests config gen rnd1 ntest (nfail+1) stamps
           Just True  ->
             mytests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
           Just False ->
             putStr ( "Falsifiable after "
                   ++ show ntest
                   ++ " tests:\n"
                   ++ unlines (arguments result)
                    ) >> hFlush stdout >> return (False, ntest)
     where
      result      = generate (configSize config ntest) rnd2 gen
      (rnd1,rnd2) = split rnd0

done :: String -> Int -> [[String]] -> IO ()
done mesg ntest stamps = putStr ( mesg ++ " " ++ show ntest ++ " tests" ++ table )
  where
    table = display
            . map entry
            . reverse
            . sort
            . map pairLength
            . group
            . sort
            . filter (not . null)
            $ stamps

    display []  = ".\n"
    display [x] = " (" ++ x ++ ").\n"
    display xs  = ".\n" ++ unlines (map (++ ".") xs)

    pairLength xss@(xs:_) = (length xss, xs)
    entry (n, xs)         = percentage n ntest
                       ++ " "
                       ++ concat (intersperse ", " xs)

    percentage n m        = show ((100 * n) `div` m) ++ "%"

------------------------------------------------------------------------



main :: IO ()
main = do
    args <- getArgs
    let n = if null args then 100 else read (head args)

    (results, passed) <- liftM unzip $
        foldM ( \prev (name,subtests) -> do
                     printf "\n%s\n" name
                     printf "%s\n" $ replicate (length name) '-'
                     cur <- mapM (\(s,a) -> printf "%-30s: " s >> a n) subtests
                     return (prev ++ cur)
              )
              []
              tests

    printf "\nPassed %d tests!\n\n" (sum passed)
    when (not . and $ results) $ fail "\nNot all tests passed!"
 where

    tests = [ ("Walker"  , tests_Walker)
            ]
