module Inserter ( inserter, Domain(..), defaultDomain ) where

import Data.Conduit
import Data.Aeson (ToJSON, toJSON, object, encode)
import Control.Monad.IO.Class
import Control.Lens
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString.Lazy (toStrict)

import qualified Network.Etcd as NE

import System.Log.Logger

import Runtime

data Domain = Domain
              { _domainPath :: !T.Text
              , _domainHost :: !T.Text
              , _domainPort :: !Int
              , _domainPriority :: !Int
              , _domainWeight :: !Int
              , _domainTtl :: !Int
              } deriving (Show, Eq)

makeFields ''Domain
makeFields ''Config
makeFields ''Runtime

defaultDomain :: Runtime -> Domain
defaultDomain r = Domain "" "" 0 10 20 (r ^. config . skydnsTTL)

instance ToJSON Domain where
  toJSON d = object [ ("host", toJSON $ d ^. host)
                    , ("port", toJSON $ d ^. port)
                    , ("priority", toJSON $ d ^. priority)
                    , ("weight", toJSON $ d ^. weight)
                    , ("ttl", toJSON $ d ^. ttl)
                    ]

inserter :: Runtime -> Sink Domain IO ()
inserter r = do
  client <- liftIO $ NE.createClient [r ^. config . etcdUrl]
  awaitForever $ \d -> do
    liftIO $ infoM "etcd.inserter" $
      "Setting " ++ T.unpack (d ^. path) ++ " to " ++ T.unpack (encodeDomain d)
    mn <- liftIO $ NE.set client (d ^. path) (encodeDomain d) (Just $ r ^. config . etcdTTL)
    liftIO $ case mn of
     Just _ -> infoM "etcd.inserter" "Insert succeeded"
     Nothing -> warningM "etcd.inserter" $ "failed to insert node " ++ T.unpack (d ^. path)

encodeDomain :: Domain -> T.Text
encodeDomain = T.decodeUtf8 . toStrict . encode
