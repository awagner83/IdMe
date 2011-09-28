module Idme.Sync (IdChan, runSync) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan)
import Data.ByteString (hPut, hGetContents)
import Data.Map (Map)
import Data.Serialize (encode, decode)
import qualified Data.Map as Map
import System.Directory (doesFileExist)
import System.IO (withFile, IOMode(WriteMode, ReadMode))

import Idme.Transaction (IdTVar, Namespace, putId, getNs)
import Idme.Util ((>>-), condM, pairOf)


-- | Filename to load/save db
type DbFileName = String

-- | Chan through which we exchange id request/responses
type IdChan = Chan IdTVar

-- | In-memory map of namespaces & their current vals/upper-bounds
type IdMap = Map Namespace (Integer, Integer)

-- | Persistent map of namespaces/current-id value
type IdMapPersistent = Map Namespace Integer

-- | Incr Id response record
data IncrIdResp = IncrIdResp { respId  :: Integer
                             , respMap :: IdMap
                             , shouldResync :: Bool }


-- | Start id-synching thread
runSync :: DbFileName -> IO (IdChan)
runSync f = do
    initMap <- load f
    newChan >>- forkIO . loop f initMap

-- | id-synching loop
loop :: DbFileName -> IdMap -> IdChan -> IO ()
loop f m c = do
    tv <- readChan c
    ns <- getNs tv
    resp <- condM (incrId ns m) shouldResync (save f . respMap)
    putId (show $ respId resp) tv
    loop f (respMap resp) c

-- | Load map from disk
load :: DbFileName -> IO IdMap
load f = do
    exists <- doesFileExist f
    case exists of
        True -> do
            dbBs <- withFile f ReadMode hGetContents
            case decode dbBs of
                Left  _ -> fail "Error loading id-db."
                Right m -> return $ fromPersistent m
        False -> do
            putStrLn "No db present, will create shiny-new version."
            return Map.empty

-- | Save map to disk
save :: DbFileName -> IdMap -> IO ()
save f = withFile f WriteMode . flip hPut . encode . toPersistent

-- | Incr step size
incrStep :: Integer
incrStep = 10

-- | Increment id value in map
incrId :: Namespace -> IdMap -> IncrIdResp
incrId k m = let (a, b) = Map.findWithDefault (0, incrStep) k m
                 a' = succ a
                 b' = if a' < b then b else (b + incrStep)
             in IncrIdResp { respId = a'
                           , respMap = Map.insert k (a', b') m
                           , shouldResync = b /= b' }

-- | Convert from persistent-map to in-memory map
fromPersistent :: IdMapPersistent -> IdMap
fromPersistent = fmap pairOf

-- | Convert to persistent-map from in-memory map
toPersistent :: IdMap -> IdMapPersistent
toPersistent = fmap snd

