module Inserter ( inserter, Domain(..), defaultDomain ) where

import Data.Conduit
import Data.Aeson (ToJSON, toJSON, object, encode)
import Control.Monad.IO.Class
import Control.Lens
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString.Lazy (toStrict)

import qualified Network.Etcd as NE

import System.Log.Logger

import Config

data Domain = Domain
              { _path :: !T.Text
              , _host :: !T.Text
              , _port :: !Int
              , _priority :: !Int
              , _weight :: !Int
              , _ttl :: !Int
              } deriving (Show, Eq)

makeLenses ''Domain

defaultDomain :: Config -> Domain
defaultDomain c = Domain "" "" 0 10 20 (skydnsTTL c)

instance ToJSON Domain where
  toJSON d = object [ ("host", toJSON $ _host d)
                    , ("port", toJSON $ _port d)
                    , ("priority", toJSON $ _priority d)
                    , ("weight", toJSON $ _weight d)
                    , ("ttl", toJSON $ _ttl d)
                    ]

inserter :: Config -> Sink Domain IO ()
inserter c = do
  client <- liftIO $ NE.createClient [etcdUrl c]
  awaitForever $ \d -> do
    liftIO $ infoM "etcd.inserter" $
      "Setting " ++ T.unpack (d ^. path) ++ " to " ++ T.unpack (encodeDomain d)
    mn <- liftIO $ NE.set client (d ^. path) (encodeDomain d) (Just $ etcdTTL c)
    liftIO $ case mn of
     Just _ -> infoM "etcd.inserter" "Insert succeeded"
     Nothing -> warningM "etcd.inserter" $ "failed to insert node " ++ T.unpack (d ^. path)

encodeDomain :: Domain -> T.Text
encodeDomain = T.decodeUtf8 . toStrict . encode
