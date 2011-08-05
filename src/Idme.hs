module Main where

import System.IO

import qualified Idme.Config as C
import Idme.Internal

-- |Startup as blocking process
main :: IO ()
main = let cfg = C.defaultConfig { C.dataDir = "." }
       in run logToStderr cfg

-- |Stderr logger
logToStderr :: LogFn
logToStderr msg = hPutStr stderr (msg ++ "\n")

