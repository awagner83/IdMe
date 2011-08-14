module Idme.Worker where

import Control.Concurrent
import Control.Monad (guard)
import System.IO
import System.IO.Error
import Control.Exception (tryJust)

import qualified Idme.State as S

type ClientHandle = Handle

-- | Worker thread function
worker :: S.AppState -> ClientHandle -> IO ()
worker st clientH = do
    req <- tryJust (guard . isEOFError) (hGetLine clientH)
    case req of
        Left _ -> hClose clientH >> (S.log st) "Client disconnected"
        Right _ -> do
            idInc <- modifyMVar (S.counter st) (getId $ S.txHandle st)
            hPutStr clientH $ formatId idInc
            worker st clientH


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

