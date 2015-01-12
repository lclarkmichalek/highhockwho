module Network.HighHock.Main where

import qualified Network.HighHock.Registry as R
import qualified Network.HighHock.Watcher as W
import qualified Network.HighHock.Extractor as E
import qualified Network.HighHock.Inserter as I
import qualified Network.HighHock.Controller as C
import Network.HighHock.Runtime

import Data.Conduit
import qualified Data.Text as T

import Control.Monad.IO.Class (liftIO)
import Control.Monad (foldM)

import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import Control.Concurrent (forkIO)
import System.Environment (getArgs)
import System.Random (randomRIO)
import Control.Concurrent (threadDelay)

import System.Log.Logger
import Control.Lens


makeFields ''Runtime
makeFields ''Config

defaultConfigPath :: FilePath
defaultConfigPath = "/etc/highhock.json"

main :: IO ()
main = do
  args <- getArgs
  r <- createRuntime args

  infoM "main" "Starting main watcher"

  wc <- C.newController
  let handler = Catch $ infoM "main" "stopping main controller" >> C.stop wc
  mapM_ (\s -> installHandler s handler Nothing) [sigINT, sigTERM]
  run wc r

run :: C.Controller -> Runtime -> IO ()
run c r = do
  _ <- forkIO $ C.ticker c (r ^. config . mainTick)
  W.watchContainers r c $$ registerContainers r

registerContainers :: Runtime -> Sink [T.Text] IO ()
registerContainers run = reg' run R.newRegistry
  where reg' :: Runtime -> R.Registry -> Sink [T.Text] IO ()
        reg' run r = do
          ids <- await
          case ids of
           Nothing -> return ()
           Just ids' -> do
             r' <- foldM (\r f -> liftIO $ f r) r
                   [ R.removeStoppedContainers
                   , R.removeMissingContainers ids'
                   , R.insertMissingContainers (startContainerWatcher run) ids'
                   ]
             reg' run r'

startContainerWatcher :: Runtime -> T.Text -> C.Controller -> IO ()
startContainerWatcher r i c = do
  -- Sleep a random amount of time to avoid thundering herd
  randomRIO (0, r ^. config . containerTick) >>= threadDelay
  _ <- forkIO $ C.ticker c (r ^. config . containerTick)
  watcher $$ extractor =$ inserter
  where watcher = W.watchContainer c r i
        extractor = E.envVarExtract r
        inserter = I.inserter r
