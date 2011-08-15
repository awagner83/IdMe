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
    initialId <- getPersistentId cfg
    txHandle <- getFile (C.txFile cfg) AppendMode
    socket <- listenOn $ PortNumber portNum
    counter <- newMVar initialId

    logFn $ "Starting on port " ++ (show portNum)
    serverLoop $ S.AppState { S.log = logFn, S.txHandle = txHandle
                            , S.socket = socket, S.counter = counter }


-- | Main connection accepting loop
serverLoop :: S.AppState -> IO ()
serverLoop st = do
    (clientH, _, _) <- accept $ S.socket st
    _ <- forkIO $ hSetBuffering clientH LineBuffering >> worker st clientH
    serverLoop st


-- | Get inital count value
getPersistentId :: C.Config -> IO Integer
getPersistentId cfg =
    let tx = C.txFile cfg
    in withFile tx ReadMode hFileSize


-- | Get file handle and set buffering to None
getFile :: String -> IOMode -> IO Handle
getFile name mode = do
    txHandle <- openFile name mode
    hSetBuffering txHandle NoBuffering
    return txHandle

