module Registry (
  Registry, newRegistry, applyContainer, insertContainer, removeContainer,
  removeMissingContainers, insertMissingContainers,
  ) where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Control.Concurrent as CC

import qualified Controller as C

import Control.Monad (foldM)

import System.Log.Logger

type Registry = M.Map T.Text (C.Controller, CC.ThreadId)

newRegistry :: Registry
newRegistry = M.empty

applyContainer :: (C.Controller -> IO a) -> Registry -> T.Text -> IO (Maybe a)
applyContainer f r id = case M.lookup id r of
  Just (c, _) -> fmap Just $ f c
  Nothing -> return Nothing

-- | Inserts a new container to be watched. If there is already an entry for the
-- container, will stop it
insertContainer :: Registry -> (C.Controller -> IO ()) -> T.Text -> IO Registry
insertContainer r act id = do
  c <- C.newController
  applyContainer C.stop r id
  tid <- CC.forkIO $ act c
  infoM "insertContainer" ("Inserting " ++ T.unpack id)
  return $ M.insert id (c, tid) r

removeContainer :: Registry -> T.Text -> IO Registry
removeContainer r id = do
  applyContainer C.stop r id
  infoM "removeContainer" ("Removeing " ++ T.unpack id)
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
