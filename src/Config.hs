{-# LANGUAGE DeriveGeneric #-}
module Config where

import Control.Monad (mzero)
import Control.Applicative ((<*>), (<$>))
import qualified Data.Text as T
import Data.Aeson

import qualified Network.Docker.Types as D

data Config = Config
              { etcdUrl :: !T.Text
              , etcdTTL :: !Int
              , skydnsDomain :: !T.Text
              , skydnsTTL :: !Int
              , dockerVersion :: !String
              , dockerUrl :: !String
              , publicHost :: !(Maybe T.Text)
              , publicInterface :: !(Maybe String)
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

dockerClientOpts :: Config -> D.DockerClientOpts
dockerClientOpts c = D.DockerClientOpts (dockerVersion c) (dockerUrl c)
