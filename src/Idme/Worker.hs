module Idme.Worker where

import Control.Concurrent
import Control.Monad (guard)
import System.IO
import System.IO.Error
import Control.Exception (tryJust)

type LogFn = (String -> IO ())


-- | Worker thread function
worker :: LogFn -> Handle -> Handle -> MVar Integer -> IO ()
worker logFn txHandle clientH counter = do
    req <- tryJust (guard . isEOFError) (hGetLine clientH)
    case req of
        Left _ -> hClose clientH >> logFn "Client disconnected"
        Right _ -> do
            idInc <- modifyMVar counter (getId txHandle)
            hPutStr clientH $ formatId idInc
            worker logFn txHandle clientH counter


-- | Get next counter-id in sequence
--   Access to getId is synchronized with modifyMVar (see 'worker')
--   Tx logging and cleanup are also managed here.
getId :: Handle -> Integer -> IO (Integer, Integer)
getId txHandle start = do
    let new = start + 1
    hPutChar txHandle '.'
    return (new, new)


-- | Friendly representation of id
formatId :: Integer -> String
formatId x = (show x) ++ "\n"

