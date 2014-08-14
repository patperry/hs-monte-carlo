
import Control.Monad
import Control.Monad.Primitive( PrimMonad )
import Control.Monad.MC
import Control.Monad.ST( runST )
import Data.List( foldl' )
import Data.Summary
import Text.Printf( printf )


-- | There a three items on the menu.
data Item = Cheeseburger | Fries | Milkshake


-- | A customer orders some number of items
data Customer = Customer { orderOf :: [Item] }


-- | The order size is a Poisson random variable with mean 2.
orderSize :: (PrimMonad m) => MC m Int
orderSize = liftM (1+) $ poisson 2


-- | The items are sampled with the given weights.
item :: (PrimMonad m) => MC m Item
item = sampleWithWeights [ (4, Cheeseburger), (2, Fries), (1, Milkshake) ]


-- | Generate a random order.
order :: (PrimMonad m) => MC m [Item]
order = do
    n <- orderSize
    replicateM n item


-- | Generate a random customer.
customer :: (PrimMonad m) => MC m Customer
customer = liftM Customer order


-- | A customer event.  The interarrival time is the time that elapeses
-- between when the previous customer arrives and when the current customer 
-- arrives.
data CustomerEvent = CustomerEvent { customerOf       :: !Customer
                                   , interarrivalTime :: !Double
                                   }


-- | Generate a random customer event.  The interarrival time distribution
-- is exponential with mean 1.
customerEvent :: (PrimMonad m) => MC m CustomerEvent
customerEvent = do
    c     <- customer
    delta <- exponential 10
    return $ CustomerEvent c delta


-- | The time it takes to make an item.
cook :: (PrimMonad m) => Item -> MC m Double
cook Cheeseburger = exponential 3
cook Fries        = exponential 1
cook Milkshake    = exponential 2


-- | The time it takes to cook all of the items in the list is equal
-- to the maximum time.
cookAll :: (PrimMonad m) => [Item] -> MC m Double
cookAll items = do
    ts <- mapM cook items
    return $ foldl' max 0 ts


-- | A customer in line, along with how long they have been waiting.
data Waiting = Waiting { waiting        :: !Customer
                       , hasBeenWaiting :: !Double
                       }


-- | A customer, along with how long it takes to prepare the customer's order
-- and how long the customer has to wait.
data Service = Service { serving     :: !Customer
                       , waitingTime :: !Double
                       , serviceTime :: !Double
                       }


-- | Given a customer who has been wating in line, provide them with service.
-- If the customer has been waiting for longer than 5 minutes, work twice as
-- fast to cook the food.
serveWaiting :: (PrimMonad m) => Waiting -> MC m Service
serveWaiting (Waiting c w) = do
    t <- cookAll $ orderOf c
    let t' = if w > 5 then 0.5*t else t
    return $ Service c w t'


-- | A resturant has one server, who may be busy. There is a list of
-- customers wating in line.
data Restaurant = Restaurant { inProgress  :: Maybe InProgress
                             , waitingLine :: [Waiting]
                             }


-- | An in-progress service event.
data InProgress = InProgress { service      :: !Service
                             , timeToFinish :: !Double
                             }


-- | An empty restaurant.
emptyRestaurant :: Restaurant
emptyRestaurant = Restaurant Nothing []


-- | Update the amount of time the customers have been waiting by adding
-- the given amount.
addToWait :: Double -> [Waiting] -> [Waiting]
addToWait delta = map (\(Waiting w t) -> Waiting w (t+delta))


-- | Serve customers in the restaurant for the given amount of time.        
serveForTime :: (PrimMonad m) => Double
                              -> Restaurant
                              -> MC m ([Service], Restaurant)
serveForTime =
    let serveForTimeHelp ss t r = case r of
            -- When no one is being served and no one is in line, do nothing.
            Restaurant Nothing  [] ->
                return $ (ss, r)

            -- When no one is being served, take the first person in line
            -- and start cooking their order.
            Restaurant Nothing  (x:xs) -> do
                s <- serveWaiting x
                let y = Just $ InProgress s $ serviceTime s
                serveForTimeHelp ss t $ Restaurant y xs

            -- When somone is being served, serve them for the given amount
            -- of time.  If we have enough time, finish serving them and
            -- update the amount of time everyone else has had to wait.
            -- Otherwise, just update the time to finish serving and
            -- update the waiting times of the customers in line.
            Restaurant (Just (InProgress s delta)) xs ->
                if delta <= t then let t'  = t - delta
                                       xs' = addToWait delta xs
                                       r'  = Restaurant Nothing xs' in
                                   serveForTimeHelp (ss ++ [s]) t' r'
                              else let delta' = delta - t
                                       y'     = Just $ InProgress s delta'
                                       xs'    = addToWait t xs
                                       r'     = Restaurant y' xs' in
                                   return (ss,r')
    in serveForTimeHelp []


-- | Given a new customer arrival event, produce a list of all of the new
-- service events that happen before the customer gets there, and return
-- the updated restaurant state at the time immediately after the customer
-- arrives.
processEvent :: (PrimMonad m) => CustomerEvent
                              -> Restaurant
                              -> MC m ([Service], Restaurant)
processEvent (CustomerEvent c t) r = do
    (ss,(Restaurant y xs)) <- serveForTime t r
    return $ (ss, (Restaurant y $ xs ++ [Waiting c 0]))


-- | Finish serving all of the customers in line.
finishServing :: (PrimMonad m) => Restaurant -> MC m [Service]
finishServing r = do
    (ss,_) <- serveForTime infinity r
    return ss
  where
    infinity = 1/0


-- | Run a restaurnt.  Whenever a new set of service events is generated,
-- update the accumulator.
foldRestaurant :: (PrimMonad m) => (a -> [Service] -> MC m a)
                                -> a
                                -> [CustomerEvent]
                                -> Restaurant
                                -> MC m a
foldRestaurant f a []     r = finishServing r >>= f a
foldRestaurant f a (c:cs) r = do
    (ss,r') <- processEvent c r
    a' <- f a ss
    foldRestaurant f a' cs r'


-- | Compute a summary of the total waiting times for each customer.
summarizeService :: (PrimMonad m) => [CustomerEvent] -> Restaurant -> MC m Summary
summarizeService cs r =
    foldRestaurant (\s ss -> return $! foldl' update s $ map totalTime ss)
                   empty cs r
  where
    totalTime (Service _ w s) = w+s


-- | An infinite stream of customerEvents.  This stream uses its own private 
-- random number generator (mt19937 is the Mersenne-Twister algorithm).
customerEvents :: Seed -> [CustomerEvent]
customerEvents seed = repeatMC (mt19937 seed) customerEvent


-- | Given a seed for the customers and a seed for the restaurant, run the
-- simulation.
simulation :: Seed -> Seed -> Int -> Summary
simulation customerSeed restaurantSeed n = let
    cs = take n $ customerEvents customerSeed
    r  = emptyRestaurant
    in runST $ do
        rng <- mt19937 restaurantSeed
        runMC (summarizeService cs r) rng


-- | Run the program
main =
    let customerSeed    = 0
        restaurantSeed  = 100
        numTransactions = 100000
        results         = simulation customerSeed restaurantSeed numTransactions
    in do
        putStrLn ""
        putStrLn "Total Service Time:"
        putStrLn "-------------------"
        putStrLn $ show $ results
        putStrLn ""
