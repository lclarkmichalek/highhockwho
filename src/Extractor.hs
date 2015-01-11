module Extractor where

import Data.Conduit
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import qualified Network.BSD as N
import Data.Aeson.Lens
import Control.Lens
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Control.Applicative ((<|>), (<$>))
import Data.List (find)
import System.Environment (getEnvironment)
import Control.Concurrent.Async (async, wait, Async)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Read (decimal)
import Network.Socket (inet_ntoa)

import Config
import Inserter (Domain, defaultDomain)

import System.Log.Logger

makeLenses ''Domain

envVar :: T.Text
envVar = "DISCOVER"

-- This function is MUCH bigger than it should be! Christ, I feel terrible about
-- this
envVarExtract :: Config -> Conduit A.Value IO Domain
envVarExtract c = do
  mHost <- liftIO $ getPublicHost c
  liftIO $ infoM "envVarExtract" $ "Host " ++ show mHost
  case mHost of
   Just h -> awaitForever $ \obj -> let

     -- Find all the vars named DISCOVER
     isDiscover v = envVar `T.append` "=" `T.isInfixOf` v
     discoverVars = filter isDiscover $ envVars obj

     -- Get all of the (name, internal port) entries in the DISCOVER var(s)
     getInternalPort v = case T.split (== ':') v of
       [val, sPort] -> if all isDigit (T.unpack sPort)
                       then Just (val, sPort)
                       else Nothing
       _ -> Nothing
     getInternalPorts dv = mapMaybe getInternalPort entries
       where entries = T.split (== ',') . T.drop (1 + T.length envVar) $ dv
     internalPorts = concat $ map getInternalPorts discoverVars

     -- Get all the (name, external port)s
     externalPorts = mapMaybe (getExternalPort obj) internalPorts

     -- Convert the external port to a string
     portToInt :: (T.Text, T.Text) -> Maybe (T.Text, Int)
     portToInt (v, p) = fmap (\p' -> (v, p')) (eitherToMaybe $ fst <$> decimal p)
     eitherToMaybe :: Either a b -> Maybe b
     eitherToMaybe (Right a) = Just a
     eitherToMaybe _ = Nothing
     externalPorts' = mapMaybe portToInt externalPorts

     -- Create the Domain objects
     domain n p = path .~ etcdPath c n $
                  host .~ h $
                  port .~ p $
                  defaultDomain c
     domains = map (uncurry domain) externalPorts'
     in do
       liftIO $ infoM "envVarExtract" $ show discoverVars ++ " -> " ++ show domains
       mapM_ yield domains
   Nothing -> error "Could not find host"

-- Had to ask for help on #haskell with this, thank you based irc
envVars :: A.Value -> [T.Text]
envVars o = o ^.. key "Config" . key "Env" . values . _String

-- | Given the Docker inspect return value, and a (_, internal port) pair,
-- return a (_, external port) pair, where possible. Prefers tcp (because
-- docker's udp is fucked by its nat shit anyway)
getExternalPort :: A.Value -> (T.Text, T.Text) -> Maybe (T.Text, T.Text)
getExternalPort obj (n, p) = do
  pEntry <- tcpPort <|> udpPort
  external <- pEntry ^? nth 0 . key "HostPort" . _String
  return (n, external)
  where pb = obj ^? key "NetworkSettings" . key "Ports" . _Object
        tcpPort = pb >>= M.lookup (p `T.append` "/tcp")
        udpPort = pb >>= M.lookup (p `T.append` "/udp")

skydnsPrefix :: T.Text
skydnsPrefix = "skydns"

-- | Given a domain to be prepended to the authorative domain, return the etcd
-- path of the record.
-- i.e.
-- Given the config has skydnsDomain = "highhock.local.", `etcdPath c "foo"`
-- will return the record path for the "foo.highhock.local" domain
etcdPath :: Config -> T.Text -> T.Text
etcdPath c d = T.intercalate "/" (skydnsPrefix : pth)
  where pth = filter (/= "") . reverse $ (d : T.split (== '.') (skydnsDomain c))

-- | Tries each of the hostGettingMethods in turn,
getPublicHost :: Config -> IO (Maybe T.Text)
getPublicHost c = f' =<< (mapM (\f -> async $ f c) hostGettingMethods)
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
hostFromConfig = publicHost

hostFromEnvVar :: Config -> IO (Maybe T.Text)
hostFromEnvVar _ = fmap (fmap (T.pack . snd) . find ((==) "EXTERNAL_IP" . fst)) getEnvironment

-- | The interface to look up ip on if no interface configured
defaultNetworkInterface :: String
defaultNetworkInterface = "eth0"

hostFromNetworkInterface :: Config -> IO (Maybe T.Text)
hostFromNetworkInterface c =
  let ifame = case publicInterface c of
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
