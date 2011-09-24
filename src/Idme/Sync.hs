module Idme.Sync (IdChan, runSync) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan)
import Control.Concurrent.STM (TVar, atomically, writeTVar)
import Idme.Util ((>>-))

type IdTVar = TVar (Maybe Integer)
type IdChan = Chan IdTVar


-- | Start id-synching thread
runSync :: Integer -> IO (IdChan)
runSync x = newChan >>- forkIO . loop x

-- | id-synching loop
loop :: Integer -> IdChan -> IO ()
loop x c = readChan c >>= atomically . flip writeTVar (Just x)
                      >>  loop (succ x) c

