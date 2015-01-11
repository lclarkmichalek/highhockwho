module Network.HighHock.Watcher
       ( watchContainer
       , watchContainers
       ) where

import Data.Conduit
import qualified Data.Text as T
import qualified Network.HighHock.Controller as C
import qualified Data.Aeson as A
import Data.Aeson (FromJSON(..))

import qualified Network.Docker as D
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Applicative ((<$>))
import Control.Monad (mzero)
import Control.Exception (catch, SomeException)

import Network.HighHock.Runtime

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

watchContainer :: MonadIO m => C.Controller -> Runtime -> T.Text -> Source m A.Value
watchContainer c r i =
  let logger = "watchContainer." ++ T.unpack i
  in control c $ do
    liftIO $ infoM logger $ "Getting details"
    details <- liftIO $ getContainerDetails r i
    case details of
     Just det -> do
       liftIO $ infoM logger "Got details"
       yield det
     Nothing -> liftIO $ warningM logger "Failed to get details"

watchContainers :: Runtime -> C.Controller -> Source IO [T.Text]
watchContainers r c = control c $ do
  liftIO $ infoM "watchContainers" "Getting containers"
  cs <- liftIO (getContainers r)
  liftIO $ infoM "watchContainers" ("Got " ++ show (length cs) ++ " containers")
  yield cs

getContainerDetails :: Runtime -> T.Text -> IO (Maybe A.Value)
getContainerDetails r i = catch (get' $ dockerClientOpts r) handler
  where get' = D.decodeResponse . D._dockerGetQuery url
        url = T.unpack ("/containers/" `T.append` i `T.append` "/json")
        handler :: SomeException -> IO (Maybe A.Value)
        handler e =
          (errorM "getContainerDetails" $ "Failed to get container details: " ++ show e) >>
          return Nothing

getContainers :: Runtime -> IO [T.Text]
getContainers r = do
  let get' = D.decodeResponse . D._dockerGetQuery "/containers/json"
  mcs <- catch (get' $ dockerClientOpts r) handler
  case mcs of
   Just cs -> return $ map (\(BasicContainer v) -> v) cs
   Nothing -> warningM "getContainers" "Failed to get any containers" >> return []
  where
    handler :: SomeException -> IO (Maybe [BasicContainer])
    handler e =
      (errorM "getContainers" $ "Failed to get containers: " ++ show e) >>
      return Nothing
