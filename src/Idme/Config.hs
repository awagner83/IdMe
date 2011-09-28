module Idme.Config where

import Network

{-|
  Idme config data (used in Idme.Internal.run)

  portNum: port to accept connections on
  dataFile: db file name
  nodeId: optional node id to concat to end of id
  nodeIdDelim: if NodeId is not Nothing, char to seperate
        incrementing id from node id.  Eg: '123.1', where
        123 is the incrementing portion of the id
        '.' is the node-id delimiter, and
        "1" is the node-id
-}
data Config = Config { portNum     :: PortNumber
                     , dataFile    :: String
                     , nodeId      :: Maybe String
                     , nodeIdDelim :: Char }

-- |Default server configuration values
defaultConfig :: Config
defaultConfig = Config { portNum     = 8888
                       , dataFile    = "idme.db"
                       , nodeId      = Nothing
                       , nodeIdDelim = '.' }

