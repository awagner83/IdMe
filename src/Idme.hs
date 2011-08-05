module Main where

import Control.Concurrent
import Network
import System.IO
import System.Posix.Syslog

import Idme.Internal

-- Startup as blocking process
main :: IO ()
main = run

