module Tests.HighHock.Controller (controllerTests) where

import Test.Hspec
import Control.Monad (void, forM, forM_)
import Control.Concurrent.Async (async, poll)
import Control.Concurrent (threadDelay)
import Data.Maybe (isJust)

import Network.HighHock.Controller

controllerStates :: IO [Controller]
controllerStates = do
  empty <- newController
  stopped <- newController
  stop stopped
  triggered <- newController
  void $ trigger triggered
  return [empty, stopped, triggered]

controllerTests = do
  describe "Controller.stop" $ do
    it "stops the controller immedietly" $ do
      let timeout = 10 ^ 3 -- Nanos
      states <- controllerStates
      asyncs <- forM states $ \c -> async $ do
        stop c
      threadDelay timeout
      forM_ asyncs $ \a -> do
        state <- poll a
        state `shouldSatisfy` isJust

  describe "Controller.trigger" $ do
    it "does not trigger a stopped controller" $ do
      c <- newController
      stop c
      t <- trigger c
      t `shouldBe` False
      r <- wait c
      r `shouldBe` False
