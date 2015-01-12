module Network.HighHock.Controller (
  Controller,
  newController,
  stop,
  trigger,
  wait,
  isStopped,
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

-- | Doesn't block
isStopped :: Controller -> IO Bool
isStopped (Controller c) = atomically $ do
  v <- STM.tryTakeTMVar c
  case v of
   Just v' -> STM.putTMVar c v'
   _ -> return ()
  return $ v == Just False

ticker :: Controller -> Int -> IO ()
ticker c i =
  trigger c >>= \s ->
  when s $ threadDelay i >> ticker c i
