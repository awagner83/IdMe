module Idme.Manager where

import Control.Concurrent
import Network
import System.IO

import qualified Idme.Config as C
import Idme.Worker (worker)

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


-- | Get transaction log handler and set buffering mode
getTxFile :: C.Config -> IO Handle
getTxFile cfg = do
    txHandle <- openFile (C.txFile cfg) AppendMode
    hSetBuffering txHandle NoBuffering
    return txHandle

