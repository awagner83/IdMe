module Idme.Internal where

import Control.Concurrent
import Network
import System.IO
import System.Posix.Syslog

type NodeId = String
type IdSequence = [String]

-- Char dividing incrementing-id from node-id
idNodeDelim :: Char
idNodeDelim = '.'

-- Init system and kick-off main loop
run :: IO ()
run = do
    txHandle <- openFile "txlog" AppendMode
    hSetBuffering txHandle NoBuffering
    socket <- listenOn $ PortNumber 8888

    syslog Info "Starting on port 8888"
    idLoop (idSequence 1 "1") txHandle socket

-- Main id-serving loop
idLoop :: IdSequence -> Handle -> Socket -> IO b
idLoop (id:ids) txHandle sock = do
    hPutChar txHandle '.'
    (clientH, _, _) <- accept sock

    -- Remaining work need not be synchronous
    forkIO $ do
        hPutStr clientH id
        hClose clientH

    idLoop ids txHandle sock

-- Sequence of ids to hand to clients.
idSequence :: Integer -> NodeId -> IdSequence
idSequence start nodeId = map format [start..]
    where format x = (show x) ++ (idNodeDelim : nodeId)

