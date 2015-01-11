module Controller (
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

data Controller = Controller (STM.TMVar Bool)

newController :: IO Controller
newController = atomically $ fmap Controller STM.newEmptyTMVar

stop :: Controller -> IO ()
stop (Controller c) = void $ atomically $ STM.swapTMVar c False

-- | Returns False if the controller has been stopped
trigger :: Controller -> IO Bool
trigger (Controller c) = atomically $ do
  v <- STM.tryTakeTMVar c
  case v of
   Just v' -> return v'
   Nothing -> STM.putTMVar c True >> return True

-- | Returns False if the controller has been 'stopped'
wait :: Controller -> IO Bool
wait (Controller c) = atomically $ STM.takeTMVar c

ticker :: Controller -> Int -> IO ()
ticker c i =
  trigger c >>= \s ->
  when s $ threadDelay i >> ticker c i
