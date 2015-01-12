module Network.HighHock.Registry
       ( Registry
       , newRegistry
       , applyContainer
       , insertContainer
       , removeContainer
       , removeMissingContainers
       , insertMissingContainers
       , removeStoppedContainers
       ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Control.Concurrent as CC

import qualified Network.HighHock.Controller as C

import Control.Monad (foldM, void)
import Control.Exception (finally)

import System.Log.Logger

type Registry = M.Map T.Text C.Controller

newRegistry :: Registry
newRegistry = M.empty

applyContainer :: (C.Controller -> IO a) -> Registry -> T.Text -> IO (Maybe a)
applyContainer f r id = case M.lookup id r of
  Just c -> fmap Just $ f c
  Nothing -> return Nothing

-- | Inserts a new container to be watched. If there is already an entry for the
-- container, will stop it
insertContainer :: Registry -> (C.Controller -> IO ()) -> T.Text -> IO Registry
insertContainer r act id = do
  c <- C.newController
  applyContainer C.stop r id
  void $ CC.forkIO $ finally (act c) (C.stop c)
  infoM "insertContainer" ("Inserting " ++ T.unpack id)
  return $ M.insert id c r

removeContainer :: Registry -> T.Text -> IO Registry
removeContainer r id = do
  applyContainer C.stop r id
  infoM "removeContainer" ("Removing " ++ T.unpack id)
  return $ M.delete id r

-- | Remove containers that are missing from the list of ids given. Stop also
removeMissingContainers :: Registry -> [T.Text] -> IO Registry
removeMissingContainers r ids = foldM removeContainer r missing
  where missing = filter (`notElem` ids) $ M.keys r

-- | Add containers that are missing from the registry (and in the list of ids given)
insertMissingContainers :: Registry -> (T.Text -> C.Controller -> IO ()) ->
                           [T.Text] -> IO Registry
insertMissingContainers r fact ids = foldM insert r missing
  where missing = filter (`notElem` M.keys r) ids
        insert r' i = insertContainer r' (fact i) i

-- | Check the controller of each entry, and remove if stopped (any thread that
-- finish will be stopped, due to our finally wrapper in the forkIO)
removeStoppedContainers :: Registry -> IO Registry
removeStoppedContainers r = dead >>= foldM removeContainer r
  where dead = M.foldlWithKey acc (return []) r
        acc :: IO [T.Text] -> T.Text -> C.Controller -> IO [T.Text]
        acc v id c = do
          v' <- v
          s <- C.isStopped c
          if s
            then return (id:v')
            else return v'
