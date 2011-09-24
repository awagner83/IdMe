module Idme.Worker (handleClient) where

import Control.Concurrent.Chan (writeChan)
import Control.Concurrent.STM (atomically, newTVar, readTVar, retry)
import Data.Maybe (fromJust)
import System.IO (Handle, hGetLine, hPutStr, hFlush)

import Idme.Log (LogChan)
import Idme.Sync (IdChan)


-- | Take client handle and deal with given requests
handleClient :: LogChan -> IdChan -> Handle -> IO ()
handleClient lChan sChan client = do
    req <- hGetLine client
    id <- wait sChan
    hPutStr client $ show id
    hFlush client
    handleClient lChan sChan client


-- | Wait for resp from Chan
wait :: IdChan -> IO Integer
wait chan = do
    -- Request new id from sync thread
    tvar <- atomically $ newTVar Nothing
    writeChan chan tvar

    -- Wait for response from channel
    fmap fromJust $ atomically $ do x <- readTVar tvar
                                    case x of Nothing -> retry
                                              Just a -> return x

