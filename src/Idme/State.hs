module Idme.State where

import Control.Concurrent (MVar())
import Network (Socket())
import System.IO (Handle())

{-|
  Application State Record.
     
  logFn: Function log things
  txHandle: Transaction-log handle
  socket: Server-socket handle
  counter: Synchronizing counter
-}
data AppState = AppState { log :: (String -> IO ())
                         , txHandle :: Handle
                         , socket :: Socket
                         , counter :: MVar Integer }

