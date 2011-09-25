module Idme.Worker (handleClient) where

import Control.Concurrent.Chan (writeChan)
import System.IO (Handle, hGetLine, hPutStr, hFlush)

import Idme.Log (LogChan)
import Idme.Sync (IdChan)
import Idme.Transaction (IdValue, Namespace, idRequest, getId)
import Idme.Util ((>>-))


-- | Take client handle and deal with given requests
handleClient :: LogChan -> IdChan -> Handle -> IO ()
handleClient lChan sChan client = do
    hPutStr client =<< wait sChan =<< hGetLine client
    hFlush client
    handleClient lChan sChan client


-- | Wait for resp from Chan
wait :: IdChan -> Namespace -> IO IdValue
wait chan ns = idRequest ns >>- writeChan chan >>= getId

