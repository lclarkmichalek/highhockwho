module Watcher (
  watchContainer, watchContainers
  ) where

import Data.Conduit
import Control.Lens
import qualified Data.Text as T
import qualified Controller as C
import qualified Data.Aeson as A
import Data.Aeson (FromJSON(..))

import qualified Network.Docker as D
import Network.Docker.Types (DockerContainer, ResourceId)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Applicative ((<$>))
import Control.Monad (mzero)
import Config

import System.Log.Logger

data BasicContainer = BasicContainer !T.Text

instance A.FromJSON BasicContainer where
  parseJSON (A.Object v) = BasicContainer <$> v A..: "Id"
  parseJSON _ = mzero

control :: MonadIO m => C.Controller -> Source m a -> Source m a
control c v = do
  running <- liftIO $ C.wait c
  if not running
    then return ()
    else v >> control c v

watchContainer :: MonadIO m => C.Controller -> Config -> T.Text -> Source m A.Value
watchContainer c conf i =
  let logger = "watchContainer." ++ T.unpack i
  in control c $ do
    liftIO $ infoM logger $ "Getting details"
    details <- liftIO $ getContainerDetails conf i
    case details of
     Just det -> do
       liftIO $ infoM logger "Got details"
       yield det
     Nothing -> liftIO $ warningM logger "Failed to get details"

watchContainers :: Config -> C.Controller -> Source IO [T.Text]
watchContainers conf c = control c $ do
  liftIO $ infoM "watchContainers" "Getting containers"
  cs <- liftIO (getContainers conf)
  liftIO $ infoM "watchContainers" ("Got " ++ show (length cs) ++ " containers")
  yield cs

getContainerDetails :: Config -> T.Text -> IO (Maybe A.Value)
getContainerDetails c i = get' $ dockerClientOpts c
  where get' = D.decodeResponse . D._dockerGetQuery url
        url = T.unpack ("/containers/" `T.append` i `T.append` "/json")

getContainers :: Config -> IO [T.Text]
getContainers c = do
  let get' = D.decodeResponse . D._dockerGetQuery "/containers/json"
  mcs <- get' $ dockerClientOpts c
  case mcs of
   Just cs -> return $ map (\(BasicContainer v) -> v) cs
   Nothing -> warningM "getContainers" "Failed to get any containers" >> return []
