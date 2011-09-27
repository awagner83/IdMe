module Idme.Transaction ( IdTransaction()
                        , IdTVar
                        , IdValue
                        , Namespace
                        , idRequest
                        , getId
                        , putId
                        , getNs
                        ) where

import Control.Concurrent.STM (STM, atomically, retry)
import Control.Concurrent.STM.TVar


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

-- | Extract Namespace from IdTransaction value
getNs :: IdTVar -> IO Namespace
getNs t = atomically (readTVar t >>= attemptNsGet)

-- | Extract ID from IdTransaction value
getId :: IdTVar -> IO IdValue
getId t = atomically (readTVar t >>= attemptIdGet)

-- | Write new ID to IdTransaction TVar value
putId :: IdValue -> IdTVar -> IO ()
putId x = atomically . flip writeTVar (Response x)

-- | Attempt to fetch ns from STM monad
attemptNsGet :: IdTransaction -> STM Namespace
attemptNsGet (Response _) = retry
attemptNsGet (Request  a) = return a

-- | Attempt to fetch id from STM monad
attemptIdGet :: IdTransaction -> STM IdValue
attemptIdGet (Request  _) = retry
attemptIdGet (Response a) = return a

