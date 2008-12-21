module Main where
    
import Control.Monad
import Control.Monad.MC
import Data.List
import Data.Map( Map )
import qualified Data.Map as Map
import System.Environment
import Text.Printf
    
-- | First we define data types for representing cards.  An Ace has
-- 'number' equal to @1@.  Jack, Queen, and King have numbers @11@, 
-- @12@, and @13@, respectively.
data Suit = Club | Diamond  | Heart | Spade deriving (Eq, Show, Ord)
data Card = Card { number :: !Int 
                 , suit   :: !Suit
                 }
          deriving (Eq, Show, Ord)

-- | Now we define a type for the various poker hands
data Hand = HighCard  | Pair | TwoPair | ThreeOfAKind | Straight | Flush
          | FullHouse | FourOfAKind | StraightFlush 
          deriving (Eq, Show, Ord)

-- | Determine the hand corresponding to a list of five cards.
hand :: [Card] -> Hand
hand cs = 
    case matches of 
        [1,1,1,1,1] -> case undefined of
                           _ | isStraight && isFlush -> StraightFlush
                           _ | isFlush               -> Flush
                           _ | isStraight            -> Straight
                           _ | otherwise             -> HighCard
        [1,1,1,2]                                    -> Pair
        [1,2,2]                                      -> TwoPair
        [1,1,3]                                      -> ThreeOfAKind
        [2,3]                                        -> FullHouse
        [1,4]                                        -> FourOfAKind
  where
    isStraight =
        let c':cs' = sort cs in
        isStraight' cs' && (  number c' == number (head cs') - 1
                           || number c' == 1 && number (head cs') == 10)
      where
        isStraight' []                            = True
        isStraight' [x]                           = True
        isStraight' (x1:x2:xs) = 
            number x2 == number x1 + 1 && isStraight' (x2:xs)
        
    isFlush = all (== suit (head cs)) $ map suit (tail cs)
    
    matches = (sort . map length . group . sort . map number) cs
    
-- | Get a list of cards that make up a 52-card deck.
deck :: [Card]
deck = [ Card i s | i <- [1..13], s <- [ Club, Diamond, Heart, Spade ] ]

-- | We deal a five-card hand by choosing a random subset of the deck.
deal :: (MonadMC m) => m [Card]
deal = sampleSubset 5 52 deck


type HandCounts = Map Hand Int

emptyCounts :: HandCounts
emptyCounts = Map.empty

updateCounts :: HandCounts -> [Card] -> HandCounts
updateCounts counts cs = Map.insertWith' (+) (hand cs) 1 counts


main = do
    [reps] <- map read `fmap` getArgs
    main' reps

main' reps =
    let seed   = 0
        counts = repeatMCWith updateCounts emptyCounts reps deal
                 `evalMC` mt19937 seed in do
    printf "\n"
    printf "%-13s %-7s   %-7s   %-14s\n" 
        "    Hand" " Count" "Probability" "  95% Interval"
    printf "-------------------------------------------------------\n"
    forM_ ((reverse . Map.toAscList) counts) $ \(h,c) ->
        let p     = fromIntegral c / fromIntegral reps
            (l,u) = interval p in
        printf "%-13s %7d   %.8f  (%.6f,%.6f)\n" (show h) c p l u
    printf "\n"
  where
    interval p = 
        let se    = sqrt (p * (1 - p) / fromIntegral reps)
            delta = 1.959964 * se in
        (max (p-delta) 0, p+delta) :: (Double,Double)
            