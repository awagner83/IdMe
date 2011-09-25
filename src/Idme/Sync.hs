module Idme.Sync (IdChan, runSync) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan)

import Idme.Transaction (IdTVar, putId)
import Idme.Util ((>>-))


type IdChan = Chan IdTVar


-- | Start id-synching thread
runSync :: Integer -> IO (IdChan)
runSync x = newChan >>- forkIO . loop x

-- | id-synching loop
loop :: Integer -> IdChan -> IO ()
loop x c = readChan c >>= putId (show x) >> loop (succ x) c

