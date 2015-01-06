module Main where

import qualified Registry as R
import qualified Watcher as W
import qualified Extractor as E
import qualified Inserter as I
import qualified Controller as C
import Config

import Data.Conduit
import qualified Data.Text as T

import Control.Monad.IO.Class (liftIO)
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import Control.Concurrent (forkIO)

import Data.Aeson (eitherDecode)
import qualified Data.ByteString.Lazy as BS (readFile)
import System.Environment (getArgs)

import System.Log.Logger
import System.Log.Handler.Simple
import System.IO (stderr)

defaultConfigPath :: FilePath
defaultConfigPath = "/etc/highhock.json"

main :: IO ()
main = do
  args <- getArgs
  conf <- case args of
   ["--config", pth] -> getConfig pth
   [] -> getConfig defaultConfigPath
   _ -> error "Invalid argument (--config supported)"

  setupLogging conf
  infoM "main" "Starting main watcher"

  wc <- C.newController
  let handler = Catch $ C.stop wc
  mapM_ (\s -> installHandler s handler Nothing) [sigINT, sigTERM]
  run wc conf

setupLogging :: Config -> IO ()
setupLogging _ = do
  s <- verboseStreamHandler stderr DEBUG
  updateGlobalLogger rootLoggerName (addHandler s . setLevel DEBUG . removeHandler)

-- Microseconds
containersTickInterval :: Int
containersTickInterval = 1000000 * 10

containerTickInterval :: Int
containerTickInterval = 1000000 * 10

run :: C.Controller -> Config -> IO ()
run c conf = do
  _ <- forkIO $ C.ticker c containersTickInterval
  W.watchContainers conf c $$ registerContainers conf

getConfig :: FilePath -> IO Config
getConfig p = do
  contents <- BS.readFile p
  case eitherDecode contents of
   Left err -> error err
   Right v -> return v

registerContainers :: Config -> Sink [T.Text] IO ()
registerContainers conf = reg' conf R.newRegistry
  where reg' :: Config -> R.Registry -> Sink [T.Text] IO ()
        reg' c r = do
          ids <- await
          case ids of
           Nothing -> return ()
           Just ids' -> do
             r' <- liftIO $ R.removeMissingContainers r ids'
             r'' <- liftIO $ R.insertMissingContainers r' (watchContainer conf) ids'
             reg' c r''

watchContainer :: Config -> T.Text -> C.Controller -> IO ()
watchContainer conf i c = do
  _ <- forkIO $ C.ticker c containerTickInterval
  watcher $$ extractor =$ inserter
  where watcher = W.watchContainer c conf i
        extractor = E.envVarExtract conf
        inserter = I.inserter conf
