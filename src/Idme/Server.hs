module Idme.Server where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (writeChan)
import Network (PortID (PortNumber), Socket, accept, listenOn)
import System.IO (BufferMode (LineBuffering), Handle, hSetBuffering)

import Idme.Config (Config, dataFile)
import Idme.Sync (runSync)
import Idme.Log (LogMessage(InfoLog), runLogger, stdoutLogger)
import Idme.Worker (handleClient)


-- | Start id-server with given config
runServer :: Config -> IO ()
runServer cfg = do
    lChan <- runLogger stdoutLogger
    sChan <- runSync $ dataFile cfg
    socket <- listenOn (PortNumber 8888)

    writeChan lChan $ InfoLog "Starting things up!"
    loop socket $ (handleClient lChan sChan)

-- | Main connection accepting loop
loop :: Socket -> (Handle -> IO ()) -> IO ()
loop socket clientFn = do
    (client, _, _) <- accept socket
    (forkIO $ runClient client) >> loop socket clientFn
    where runClient h = hSetBuffering h LineBuffering >> clientFn h

