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

applyContainer :: (C.Controller -> IO a) -> T.Text -> Registry -> IO (Maybe a)
applyContainer f id r = case M.lookup id r of
  Just c -> fmap Just $ f c
  Nothing -> return Nothing

-- | Inserts a new container to be watched. If there is already an entry for the
-- container, will stop it
insertContainer :: (C.Controller -> IO ()) -> T.Text -> Registry -> IO Registry
insertContainer act id r = do
  c <- C.newController
  applyContainer C.stop id r
  void $ CC.forkIO $ finally (act c) (C.stop c)
  infoM "insertContainer" ("Inserting " ++ T.unpack id)
  return $ M.insert id c r

removeContainer :: T.Text -> Registry -> IO Registry
removeContainer id r = do
  applyContainer C.stop id r
  infoM "removeContainer" ("Removing " ++ T.unpack id)
  return $ M.delete id r

-- | Remove containers that are missing from the list of ids given. Stop also
removeMissingContainers :: [T.Text] -> Registry -> IO Registry
removeMissingContainers ids r = foldM (flip removeContainer) r missing
  where missing = filter (`notElem` ids) $ M.keys r

-- | Add containers that are missing from the registry (and in the list of ids given)
insertMissingContainers :: (T.Text -> C.Controller -> IO ()) -> [T.Text]
                           -> Registry -> IO Registry
insertMissingContainers fact ids r = foldM insert r missing
  where missing = filter (`notElem` M.keys r) ids
        insert r' i = insertContainer (fact i) i r'

-- | Check the controller of each entry, and remove if stopped (any thread that
-- finish will be stopped, due to our finally wrapper in the forkIO)
removeStoppedContainers :: Registry -> IO Registry
removeStoppedContainers r = dead >>= foldM (flip removeContainer) r
  where dead = M.foldlWithKey acc (return []) r
        acc :: IO [T.Text] -> T.Text -> C.Controller -> IO [T.Text]
        acc v id c = do
          v' <- v
          s <- C.isStopped c
          if s
            then return (id:v')
            else return v'
