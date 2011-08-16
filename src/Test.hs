{-# OPTIONS_GHC -F -pgmF htfpp #-}

import GHC.IO.Exception (ExitCode())
import System.Environment (getArgs)
import Test.Framework

-- | Entry point for test runner.
--   Mysterious 'allHTFTests' name is added by HTF preprocessor
main :: IO ExitCode
main = getArgs >>= flip runTestWithArgs allHTFTests

