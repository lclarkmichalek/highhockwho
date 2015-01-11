module Main where

import qualified Registry as R
import qualified Watcher as W
import qualified Extractor as E
import qualified Inserter as I
import qualified Controller as C
import Runtime

import Data.Conduit
import qualified Data.Text as T

import Control.Monad.IO.Class (liftIO)
import System.Posix.Signals (installHandler, Handler(Catch), sigINT, sigTERM)
import Control.Concurrent (forkIO)

import System.Environment (getArgs)
import System.Log.Logger


defaultConfigPath :: FilePath
defaultConfigPath = "/etc/highhock.json"

main :: IO ()
main = do
  args <- getArgs
  r <- createRuntime args

  infoM "main" "Starting main watcher"

  wc <- C.newController
  let handler = Catch $ C.stop wc
  mapM_ (\s -> installHandler s handler Nothing) [sigINT, sigTERM]
  run wc r

-- Microseconds
containersTickInterval :: Int
containersTickInterval = 1000000 * 10

containerTickInterval :: Int
containerTickInterval = 1000000 * 10

run :: C.Controller -> Runtime -> IO ()
run c r = do
  _ <- forkIO $ C.ticker c containersTickInterval
  W.watchContainers r c $$ registerContainers r

registerContainers :: Runtime -> Sink [T.Text] IO ()
registerContainers run = reg' run R.newRegistry
  where reg' :: Runtime -> R.Registry -> Sink [T.Text] IO ()
        reg' run r = do
          ids <- await
          case ids of
           Nothing -> return ()
           Just ids' -> do
             r' <- liftIO $ R.removeMissingContainers r ids'
             r'' <- liftIO $ R.insertMissingContainers r' (startContainerWatcher run) ids'
             reg' run r''

startContainerWatcher :: Runtime -> T.Text -> C.Controller -> IO ()
startContainerWatcher r i c = do
  _ <- forkIO $ C.ticker c containerTickInterval
  watcher $$ extractor =$ inserter
  where watcher = W.watchContainer c r i
        extractor = E.envVarExtract r
        inserter = I.inserter r
