module Network.HighHock.Extractor where

import Data.Conduit
import qualified Data.Aeson as A
import qualified Data.Text as T
import qualified Data.HashMap.Strict as M
import Data.Aeson.Lens
import Control.Lens
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Control.Applicative ((<|>), (<$>))
import Control.Monad.IO.Class (liftIO)
import Data.Text.Read (decimal)

import Network.HighHock.Runtime
import Network.HighHock.Inserter (Domain, defaultDomain)

import System.Log.Logger

makeFields ''Domain
makeFields ''Runtime
makeFields ''Config

envVar :: T.Text
envVar = "DISCOVER"

-- This function is MUCH bigger than it should be! Christ, I feel terrible about
-- this
envVarExtract :: Runtime -> Conduit A.Value IO Domain
envVarExtract r = awaitForever $ \obj -> let
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
  domain n p = path .~ etcdPath (r ^. config) ((r ^. hostname) `T.append` n) $
               host .~ (r ^. publicHost) $
               port .~ p $
               defaultDomain r
  domains = map (uncurry domain) externalPorts'
  in do
    liftIO $ infoM "envVarExtract" $ show discoverVars ++ " -> " ++ show domains
    mapM_ yield domains

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
  where pth = filter (/= "") . reverse $ (d : T.split (== '.') (c ^. skydnsDomain))
