module Idme.Log ( LogMessage(ErrorLog, InfoLog)
                , LogChan
                , runLogger
                , stdoutLogger ) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan)
import Idme.Util ((>>-))


type Logger = LogMessage -> IO ()
type LogChan = Chan LogMessage

data LogMessage = ErrorLog String | InfoLog String

instance Show LogMessage where
    show (ErrorLog x) = "Error: " ++ x
    show (InfoLog  x) = "Info: "  ++ x


-- | Start logging thread
runLogger :: Logger -> IO (Chan LogMessage)
runLogger l = newChan >>- forkIO . loop l

-- | Main logger loop
loop :: Logger -> Chan LogMessage -> IO ()
loop l c = readChan c >>= l >> loop l c

-- | stdout logger
stdoutLogger :: Logger
stdoutLogger = putStrLn . show

