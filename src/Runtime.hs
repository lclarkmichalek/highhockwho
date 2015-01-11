module Runtime
       (Runtime(..)
       , createRuntime
       , Config(..)
       , dockerClientOpts
       , setupLogging
       ) where

import qualified Data.Text as T
import qualified Network.BSD as N
import qualified Data.ByteString.Lazy as BS
import qualified Network.Docker.Types as D
import qualified System.Log.Logger as L
import System.Log.Handler.Simple (verboseStreamHandler)
import Data.Aeson (eitherDecode, object)
import Data.List (find)
import Data.Maybe (fromMaybe)
import System.Environment (getEnvironment)
import System.Exit (exitSuccess)
import Control.Concurrent.Async (async, wait, Async)
import Network.Socket (inet_ntoa)
import System.IO (stderr)
import Control.Exception (try)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Lens

import Config

data Runtime = Runtime
               { _runtimeConfig :: !Config
               , _runtimePublicHost :: !T.Text
               , _runtimeDockerVersion :: !T.Text
               } deriving (Show, Eq)

makeFields ''Runtime
makeFields ''Config

defaultConfigPath :: String
defaultConfigPath = "/etc/highhock.json"

-- | Takes the command line arguments, returns a Runtime, or exits with a
-- pretty (user readable) error
createRuntime :: [String] -> IO Runtime
createRuntime args = do
  when ("-h" `elem` args || "--help" `elem` args) $
    putStrLn argHelp >> exitSuccess

  cfg <- loadConfig args
  setupLogging cfg

  L.infoM "runtime" $ "Loaded config: " ++ show cfg
  mph <- findPublicHost cfg
  ph <- must mph "Could not find public host"
  L.noticeM "runtime" $ "Found public host: " ++ show ph
  mdv <- findDockerVersion cfg
  dv <- must mdv "Could not find docker version"
  L.noticeM "runtime" $ "Found docker version: " ++ show dv
  return $ Runtime cfg ph dv

must :: Maybe a -> String -> IO a
must (Just a) _ = return a
must _ err = fail err

loadConfig :: [String] -> IO Config
loadConfig args = do
  let cfgPath = fromMaybe defaultConfigPath (findCfgPath args)
      warn = L.warningM "loadConfig"
  mcontents <- (try $ BS.readFile cfgPath) :: IO (Either IOError BS.ByteString)
  conf <- case mcontents of
   Left err ->
     (warn $ "failed to load config from " ++ cfgPath ++ ": " ++ show err) >>
     return (object [])
   Right contents -> case eitherDecode contents of
     Left err ->
       (warn $ "Failed to read config " ++ cfgPath ++ ": " ++ err) >>
       return (object [])
     Right v -> return v
  return . fromArgs args .
    fromJSON conf $
    defaultConfig

findCfgPath :: [String] -> Maybe FilePath
findCfgPath ("-c":cfgPath:_) = Just cfgPath
findCfgPath ("--config":cfgPath:_) = Just cfgPath
findCfgPath (_:xs) = findCfgPath xs
findCfgPath _ = Nothing

-- TODO: Don't have access to the docker lib docs atm
findDockerVersion :: Config -> IO (Maybe T.Text)
findDockerVersion cfg = return . Just . T.pack $ cfg ^. dockerVersion

-- | Tries each of the hostGettingMethods in turn,
findPublicHost :: Config -> IO (Maybe T.Text)
findPublicHost c = f' =<< (mapM (\f -> async $ f c) hostGettingMethods)
  where f' :: [Async (Maybe T.Text)] -> IO (Maybe T.Text)
        f' (x:xs) = do
          x' <- wait x
          case x' of
           Just h -> return $ Just h
           Nothing -> f' xs
        f' [] = return Nothing

-- TODO: Aws metadata service, DO metadata service, openstack?
hostGettingMethods :: [(Config -> IO (Maybe T.Text))]
hostGettingMethods =
  [ return . hostFromConfig
  , hostFromEnvVar
  , hostFromNetworkInterface
  ]

hostFromConfig :: Config -> Maybe T.Text
hostFromConfig = view publicHost

hostFromEnvVar :: Config -> IO (Maybe T.Text)
hostFromEnvVar _ = fmap (fmap (T.pack . snd) . find ((==) "EXTERNAL_IP" . fst)) getEnvironment

hostFromNetworkInterface :: Config -> IO (Maybe T.Text)
hostFromNetworkInterface c = do
  entries <- N.getNetworkEntries True
  liftIO $ L.infoM "hostFromNetworkInterface" $ "Networks: " ++ show entries
  case find (elem (c ^. publicInterface) . allNetworkNames) entries of
   Just n -> fmap Just $ getNetworkAddr n
   Nothing -> return Nothing

allNetworkNames :: N.NetworkEntry -> [N.NetworkName]
allNetworkNames ne = N.networkName ne : N.networkAliases ne

-- This is much more complicated than it should be
getNetworkAddr :: N.NetworkEntry -> IO T.Text
getNetworkAddr = fmap T.pack . inet_ntoa . fromIntegral . N.networkAddress

-- Convinence funcs

dockerClientOpts :: Runtime -> D.DockerClientOpts
dockerClientOpts r = D.DockerClientOpts ver url
  where ver = T.unpack $ r ^. dockerVersion
        url = r ^. config . dockerUrl

setupLogging :: Config -> IO ()
setupLogging c = do
  s <- verboseStreamHandler stderr (c ^. logLevel)
  L.updateGlobalLogger L.rootLoggerName
    (L.addHandler s . L.setLevel (c ^. logLevel) . L.removeHandler)
