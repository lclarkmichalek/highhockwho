module Controller (
  Controller,
  newController,
  stop,
  trigger,
  wait,
  ticker) where

import Control.Monad (void)
import qualified Control.Concurrent as C
import qualified System.Mem.Weak as W

data Controller = Controller (C.MVar Bool)

newController :: IO Controller
newController = fmap Controller C.newEmptyMVar

stop :: Controller -> IO ()
stop (Controller c) = void $ C.putMVar c False

trigger :: Controller -> IO ()
trigger (Controller c) = void $ C.putMVar c True

-- | Returns False if the controller has been 'stopped'
wait :: Controller -> IO Bool
wait (Controller c) = C.takeMVar c

ticker :: Controller -> Int -> IO ()
ticker c i = W.mkWeakPtr c Nothing >>= ticker'
  where ticker' wc = do
          mc <- W.deRefWeak wc
          case mc of
           Just c' -> trigger c' >> C.threadDelay i >> ticker' wc
           Nothing -> return ()
