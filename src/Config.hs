module Config(Config(..)) where

import Control.Monad (mzero)
import Control.Applicative ((<*>), (<$>))
import qualified Data.Text as T
import Data.Aeson

data Config = Config
              { _configEtcdUrl :: !T.Text
              , _configEtcdTTL :: !Int
              , _configSkydnsDomain :: !T.Text
              , _configSkydnsTTL :: !Int
              , _configDockerVersion :: !String
              , _configDockerUrl :: !String
              , _configPublicHost :: !(Maybe T.Text)
              , _configPublicInterface :: !(Maybe String)
              } deriving (Show, Eq)

instance FromJSON Config where
  parseJSON (Object v) =
    Config <$>
    v .: "etcdUrl" <*>
    v .: "etcdTTL" <*>
    v .: "skydnsDomain" <*>
    v .: "skydnsTTL" <*>
    v .: "dockerVersion" <*>
    v .: "dockerURL" <*>
    v .:? "publicHost" <*>
    v .:? "publicInterface"
  parseJSON _ = mzero
