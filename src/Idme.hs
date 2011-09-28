module Main where

import Idme.Config (defaultConfig)
import Idme.Server (runServer)

-- | Startup as blocking process
main :: IO ()
main = runServer defaultConfig

