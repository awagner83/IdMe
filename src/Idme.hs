module Main where

import System.IO

import Idme.Internal

-- Startup as blocking process
main :: IO ()
main = run logToStderr

logToStderr :: LogFn
logToStderr msg = hPutStr stderr (msg ++ "\n")

