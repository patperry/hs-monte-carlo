
import Control.Monad.MC
import Control.Monad
import Data.List( foldl' )
import System.Environment( getArgs )
import Text.Printf( printf )

data Order = 
newtype Customer = Customer { interarrivalTime :: Double }
newtype Server   = Server   { serviceTime :: Double }

customers :: Seed -> MC Double -> [Customer]
customers seed it = streamMC
servers   :: Seed -> MC Double -> [Server]