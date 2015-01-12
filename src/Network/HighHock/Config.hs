module Network.HighHock.Config
       ( Config(..)
       , fromJSON
       , fromArgs
       , argHelp
       , defaultConfig) where

import qualified Data.Text as T
import Data.Aeson hiding (fromJSON)
import Data.Aeson.Lens
import Control.Lens
import Data.Char (toUpper)
import Data.List (find)
import Data.Maybe (listToMaybe)
import System.IO.Unsafe (unsafePerformIO)
import System.Log.Logger (Priority(..))

data Config = Config
              { _configEtcdUrl :: !T.Text
              , _configEtcdTTL :: !Int
              , _configSkydnsDomain :: !T.Text
              , _configSkydnsTTL :: !Int
              , _configDockerVersion :: !String
              , _configDockerUrl :: !String
              , _configPublicHost :: !(Maybe T.Text)
              , _configPublicInterface :: !String
              , _configLogLevel :: !Priority
              , _configMainTick :: !Int
              , _configContainerTick :: !Int
              } deriving (Show, Eq)

makeFields ''Config

defaultConfig :: Config
defaultConfig = Config "http://127.0.0.1:4001/" 60 "skydns.local" 60 "v1.16"
                "http://127.0.0.1:2375/" Nothing "eth0" NOTICE 10 10

(.?~) :: ASetter s s a a -> (Maybe a) -> s -> s
(.?~) s (Just a) = s .~ a
(.?~) _ _ = id

infixl 7 .?~

(.??~) :: ASetter s s (Maybe a) (Maybe a) -> (Maybe a) -> s -> s
(.??~) s (Just a) = s .~ Just a
(.??~) _ _ = id

infixl 7 .??~

fromJSON :: Value -> Config -> Config
fromJSON = flip (foldl (\v f -> f v)) . jsonExtractors

jsonExtractors :: Value -> [Config -> Config]
jsonExtractors o =
  [ etcdUrl .?~ o ^? key "etcd" . key "url" . _String
  , etcdTTL .?~ o ^? key "etcd" . key "ttl" . _Integral
  , skydnsDomain .?~ o ^? key "skdns" . key "domain" . _String
  , skydnsTTL .?~ o ^? key "skydns" . key "ttl" . _Integral
  , dockerVersion .?~ o ^? key "docker" . key "version" . _String .to T.unpack
  , dockerUrl .?~ o ^? key "docker" . key "url" . _String .to T.unpack
  , mainTick .?~ o ^? key "docker" . key "poll" . _Integral .to (* 10^6)
  , containerTick .?~ o ^? key "docker" . key "pollContainer" . _Integral .to (* 10^6)
  , publicHost .??~ o ^? key "discovery" . key "host" . _String
  , publicInterface .?~ o ^? key "discovery" . key "interface" . _String .to T.unpack
  , logLevel .?~ (o ^? key "logLevel" . _String .to T.unpack >>= maybeRead)
  ]

fromArgs :: [String] -> Config -> Config
fromArgs (x:y:xs) =
  case find (\v -> ("-" ++ v ^. _1 == x) || ("--" ++ v ^. _2 == x)) binHandlers of
   Just (_, _, h) -> h y . (fromArgs xs)
   Nothing -> unsafePerformIO $ do
     putStrLn $ "unrecognized argument: " ++ x
     return $ fromArgs (y:xs)
fromArgs [x] = unsafePerformIO $ (putStrLn $ "unrecognized argument: " ++ x) >> return id
fromArgs [] = id

binHandlers :: [(String, String, String -> Config -> Config)]
binHandlers =
  [ ("h", "help", \_ -> id)
  , ("c", "config", \_ -> id)
  , ("e", "etcd-url", \url -> etcdUrl .~ T.pack url)
  , ("ttl", "etcd-ttl", \ttl -> etcdTTL .~ read ttl)
  , ("sd", "skydns-domain", \d -> skydnsDomain .~ T.pack d)
  , ("st", "skydns-ttl", \t -> skydnsTTL .~ read t)
  , ("dv", "docker-version", \v -> dockerVersion .~ v)
  , ("d", "docker-url", \u -> dockerUrl .~ u)
  , ("p", "docker-poll", \v -> mainTick .~ (read v * 10 ^ 6))
  , ("cp", "docker-poll-container", \v -> containerTick .~ (read v * 10 ^ 6))
  , ("p", "discovery-host", \h -> publicHost .~ (Just $ T.pack h))
  , ("i", "discovery-iface", \i -> publicInterface .~ i)
  , ("l", "log-level", \l -> logLevel .~ read l)
  ]

argHelp :: String
argHelp = header ++ "\n\n" ++ perArg binHandlers
  where header = "highhock " ++ shortOpts binHandlers
        shortOpts ((s, l, _):xs) = "[-" ++ s ++ " " ++ map toUpper l ++ "] " ++
                                   shortOpts xs
        shortOpts _ = ""
        perArg ((s, l, _):xs) = "\t-" ++ s ++ "/--" ++ l ++ "\n" ++ perArg xs
        perArg _ = ""

maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . reads
