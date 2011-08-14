module Idme.Manager where

import Control.Concurrent
import Network
import System.IO

import qualified Idme.Config as C
import qualified Idme.State as S
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
    serverLoop $ S.AppState { S.log = logFn, S.txHandle = txHandle
                            , S.socket = socket, S.counter = counter }


-- | Main connection accepting loop
serverLoop :: S.AppState -> IO ()
serverLoop st = do
    (clientH, _, _) <- accept $ S.socket st
    hSetBuffering clientH LineBuffering
    _ <- forkIO $ worker st clientH
    serverLoop st


-- | Get transaction log handler and set buffering mode
getTxFile :: C.Config -> IO Handle
getTxFile cfg = do
    txHandle <- openFile (C.txFile cfg) AppendMode
    hSetBuffering txHandle NoBuffering
    return txHandle

