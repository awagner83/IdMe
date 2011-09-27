module Idme.Sync (IdChan, runSync) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan)
import Data.Map (Map)
import qualified Data.Map as Map

import Idme.Transaction (IdTVar, Namespace, putId, getNs)
import Idme.Util ((>>-))


type IdChan = Chan IdTVar
type IdMap  = Map Namespace Integer


-- | Start id-synching thread
runSync :: Integer -> IO (IdChan)
runSync x = newChan >>- forkIO . loop Map.empty

-- | id-synching loop
loop :: IdMap -> IdChan -> IO ()
loop m c = do
    tv <- readChan c
    ns <- getNs tv
    let x = succ $ Map.findWithDefault 0 ns m
    putId (show x) tv
    loop (Map.insert ns x m) c

