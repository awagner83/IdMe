module Main where

import System.IO

import qualified Idme.Config as C
import Idme.Manager

-- |Startup as blocking process
main :: IO ()
main = let cfg = C.defaultConfig { C.dataDir = "." }
       in runServer logToStderr cfg

-- |Stderr logger
logToStderr :: String -> IO ()
logToStderr msg = hPutStr stderr (msg ++ "\n")

