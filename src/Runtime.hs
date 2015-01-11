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
import Data.Aeson (eitherDecode)
import Data.List (find)
import System.Environment (getEnvironment)
import Control.Concurrent.Async (async, wait, Async)
import Network.Socket (inet_ntoa)
import System.Exit (exitFailure)
import System.IO (stderr)
import Control.Lens

import Config (Config(..))

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
createRuntime ["-c", cfgPath] = createRuntime' cfgPath
createRuntime ["--config", cfgPath] = createRuntime' cfgPath
createRuntime [] = createRuntime' defaultConfigPath
createRuntime _ = do
  putStrLn "highhock [-c CONFIG]"
  putStrLn ""
  putStrLn "A simple docker watcher, etcd inserter"
  exitFailure

createRuntime' :: FilePath -> IO Runtime
createRuntime' cfgPath = do
  cfg <- loadConfig cfgPath
  mph <- findPublicHost cfg
  ph <- must mph "Could not find public host"
  mdv <- findDockerVersion cfg
  dv <- must mdv "Could not find docker version"
  return $ Runtime cfg ph dv

must :: Maybe a -> String -> IO a
must (Just a) _ = return a
must _ err = fail err

loadConfig :: FilePath -> IO Config
loadConfig p = do
  contents <- BS.readFile p
  case eitherDecode contents of
   Left err -> error $ "Failed to load config file: " ++ err
   Right v -> return v

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

-- | The interface to look up ip on if no interface configured
defaultNetworkInterface :: String
defaultNetworkInterface = "eth0"

hostFromNetworkInterface :: Config -> IO (Maybe T.Text)
hostFromNetworkInterface c =
  let ifame = case c ^. publicInterface of
        Nothing -> defaultNetworkInterface
        Just name -> name
  in do
    entries <- N.getNetworkEntries True
    case find (elem ifame . allNetworkNames) entries of
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

setupLogging :: Runtime -> IO ()
setupLogging _ = do
  s <- verboseStreamHandler stderr L.DEBUG
  L.updateGlobalLogger L.rootLoggerName (L.addHandler s . L.setLevel L.DEBUG . L.removeHandler)
