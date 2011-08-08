module Idme.Manager where

import Control.Concurrent
import Control.Exception (tryJust)
import Control.Monad (guard)
import Network
import System.IO
import System.IO.Error

import qualified Idme.Config as C

type LogFn = (String -> IO ())


-- | Open files and init connection listener
runServer :: LogFn -> C.Config -> IO ()
runServer logFn cfg = do
    let portNum = C.portNum cfg
    txHandle <- getTxFile cfg
    socket <- listenOn $ PortNumber portNum
    counter <- newMVar 1

    logFn $ "Starting on port " ++ (show portNum)
    serverLoop logFn txHandle socket counter


-- | Main connection accepting loop
serverLoop :: LogFn -> Handle -> Socket -> MVar Integer -> IO ()
serverLoop logFn txHandle socket counter = do
    (clientH, _, _) <- accept socket
    hSetBuffering clientH LineBuffering
    _ <- forkIO $ worker logFn txHandle clientH counter
    serverLoop logFn txHandle socket counter


-- | Worker thread function
worker :: LogFn -> Handle -> Handle -> MVar Integer -> IO ()
worker logFn txHandle clientH counter = do
    req <- tryJust (guard . isEOFError) (hGetLine clientH)
    case req of
        Left _ -> do
            hClose clientH
            logFn "Client disconnected"
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


-- | Get transaction log handler and set buffering mode
getTxFile :: C.Config -> IO Handle
getTxFile cfg = do
    txHandle <- openFile (C.txFile cfg) AppendMode
    hSetBuffering txHandle NoBuffering
    return txHandle

