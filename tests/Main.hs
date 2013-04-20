{-# LANGUAGE RankNTypes #-}
module Main where

import Control.Monad
import Data.AEq
import Data.Monoid
import Data.Summary
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Control.Monad.MC.Walker

prop_table_probs :: Weights -> Bool
prop_table_probs (Weights n ws) =
    let table = computeTable n ws
    in all (\i -> probOf table i ~== ps !! i) [0..n-1]
  where
    ps = probsFromWeights ws

prop_table_index :: Weights -> Unif -> Bool
prop_table_index (Weights n ws) (Unif u) =
    let table = computeTable n ws
        i     = indexTable table u
    in i >= 0 && i < n && (ws !! i > 0)

tests_Walker :: Test
tests_Walker = testGroup "Walker"
    [ testProperty "table probabilities" prop_table_probs
    , testProperty "table indexing"      prop_table_index
    ]

probOf :: Table -> Int -> Double
probOf table i =
    (((sum . map ((1-) . fst) . filter ((==i) . snd))
                       (map (component table) [0..n-1]))
                       + (fst . component table) i) / fromIntegral n
  where
    n = tableSize table

prop_monoid_update_equiv :: [Double] -> [Double] -> Bool
prop_monoid_update_equiv xs ys =
    approxEqualS (summary $ xs <> ys)
                 (summary xs <> summary ys)

prop_monoid_assoc :: [Double] -> [Double] -> Bool
prop_monoid_assoc xs ys =
    let (sxs, sys) = (summary xs, summary ys)
     in (sxs <> sys) `approxEqualS` (sys <> sxs)

tests_monoid :: Test
tests_monoid = testGroup "Monoid Instance"
    [ testProperty "update equivalence" prop_monoid_update_equiv
    , testProperty "associativity" prop_monoid_assoc
    ]

------------------------------- Utility functions ---------------------------

probsFromWeights :: forall b. Fractional b => [b] -> [b]
probsFromWeights ws = let
    w  = sum ws
    ps = map (/w) ws
    in ps

approxEqualS :: Summary -> Summary -> Bool
approxEqualS a b =
    sampleSize a == sampleSize b &&
      all eq [ sampleMin, sampleMax, sampleMean, sampleVar ]
    where
        eq f = f a ~== f b

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
        n  <- choose (1, 500)
        ws <- weights n
        return $ Weights n ws

data Unif = Unif Double deriving Show
instance Arbitrary Unif where
    arbitrary            = liftM Unif unif


main :: IO ()
main = defaultMain [ tests_Walker, tests_monoid ]
