module Network.HighHock.Controller (
  Controller,
  newController,
  stop,
  trigger,
  wait,
  ticker) where

import Control.Monad (void, when)
import qualified Control.Concurrent.STM.TMVar as STM
import Control.Monad.STM (atomically)
import Control.Concurrent (threadDelay)
import Data.Maybe (fromMaybe)

data Controller = Controller (STM.TMVar Bool)

newController :: IO Controller
newController = atomically $ fmap Controller STM.newEmptyTMVar

stop :: Controller -> IO ()
stop (Controller c) = void $ atomically $ do
  void $ STM.tryTakeTMVar c
  STM.putTMVar c False

-- | Returns False if the controller has been stopped
trigger :: Controller -> IO Bool
trigger (Controller c) = atomically $ do
  v <- STM.tryTakeTMVar c
  let v' = fromMaybe True v
  STM.putTMVar c v'
  return v'

-- | Returns False if the controller has been 'stopped'
wait :: Controller -> IO Bool
wait (Controller c) = atomically $ do
  v <- STM.takeTMVar c
  when (not v) $ STM.putTMVar c v
  return v

ticker :: Controller -> Int -> IO ()
ticker c i =
  trigger c >>= \s ->
  when s $ threadDelay (i * 10 ^ 6) >> ticker c i
