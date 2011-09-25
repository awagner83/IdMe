module Idme.Transaction ( IdTransaction()
                        , IdTVar
                        , IdValue
                        , Namespace
                        , idRequest
                        , getId
                        , putId
                        ) where

import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TVar
import Data.Functor ((<$>))


-- | Transaction TVar
type IdTVar = TVar IdTransaction

-- | Id-space to request id from
type Namespace = String

-- | Generated id
type IdValue = String

-- | Id req/resp transaction repr
data IdTransaction = Request Namespace | Response IdValue


-- | New request for id
idRequest :: Namespace -> IO IdTVar
idRequest = atomically . newTVar . Request

-- | Extract ID from IdTransaction value
getId :: IdTVar -> IO IdValue
getId t = unpack <$> atomically (readTVar t >>= attemptIdGet)
    where unpack (Request  _) = fail "What's up STM?  This cannot be?!"
          unpack (Response x) = x

-- | Write new ID to IdTransaction TVar value
putId :: IdValue -> IdTVar -> IO ()
putId x = atomically . flip writeTVar (Response x)

-- | Attempt to fetch id from STM monad
attemptIdGet :: IdTransaction -> STM IdTransaction
attemptIdGet (Request  _) = retry
attemptIdGet a = return a

