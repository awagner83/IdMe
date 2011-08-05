module Idme.Internal where

import Control.Concurrent
import Network
import System.IO
import System.Posix.Syslog

import qualified Idme.Config as C

type NodeId = String
type IdSequence = [String]

-- |Init system and kick-off main loop
run :: IO ()
run = do
    let cfg = C.defaultConfig { C.dataDir = "." }
        portNum = C.portNum cfg

    txHandle <- openFile (C.txFile cfg) AppendMode
    hSetBuffering txHandle NoBuffering
    socket <- listenOn $ PortNumber portNum

    syslog Info $ "Starting on port " ++ (show portNum)
    idLoop (idSequence 1 cfg) txHandle socket

-- |Main id-serving loop
idLoop :: IdSequence -> Handle -> Socket -> IO b
idLoop [] _ _ = fail "We're fresh out of ids!  Please come back later."
idLoop (x:xs) txHandle sock = do
    hPutChar txHandle '.'   -- Add a byte to the tx-log that we can count later
    (clientH, _, _) <- accept sock

    -- Remaining work need not be synchronous
    _ <- forkIO $ do
        hPutStr clientH x
        hClose clientH

    idLoop xs txHandle sock

-- |Sequence of ids to hand to clients.
idSequence :: Integer -> C.Config -> IdSequence
idSequence start cfg = map (format cfg) [start..]

-- |Format ids before returning the client (add node-id if configured)
format :: C.Config -> Integer -> String
format C.Config {C.nodeId=Nothing}                 x = show x
format C.Config {C.nodeId=Just n, C.nodeIdDelim=d} x = (show x) ++ (d:n)

