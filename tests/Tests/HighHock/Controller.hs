module Tests.HighHock.Controller (controllerTests) where

import Test.Hspec
import Control.Monad (void, forM, forM_)
import qualified Control.Concurrent.Async as A
import Control.Concurrent (threadDelay)

import Network.HighHock.Controller

import Tests.HighHock.Inst ()

controllerStates :: IO [Controller]
controllerStates = do
  empty <- newController
  stopped <- newController
  stop stopped
  triggered <- newController
  void $ trigger triggered
  return [empty, stopped, triggered]

timeout :: Int
timeout = 10 ^ 4

controllerTests = do
  describe "Controller.stop" $ do
    it "stops the controller immedietly" $ do
      states <- controllerStates
      asyncs <- forM states $ \c -> A.async $ do
        stop c
      threadDelay timeout
      forM_ asyncs $ \a -> do
        state <- A.poll a
        state `shouldBe` Just (Right ())

  describe "Controller.trigger" $ do
    it "does not trigger a stopped controller" $ do
      c <- newController
      stop c
      t <- trigger c
      t `shouldBe` False
      r <- wait c
      r `shouldBe` False

    it "does trigger an empty or triggered controller" $ do
      c <- newController
      t <- trigger c
      t `shouldBe` True
      t' <- trigger c
      t' `shouldBe` True

  describe "Controller.wait" $ do
    it "waits on a trigger" $ do
      c <- newController
      a <- A.async $ wait c
      threadDelay timeout
      void $ trigger c
      threadDelay timeout
      r <- A.poll a
      r `shouldBe` Just (Right True)

    it "waits on a stop" $ do
      c <- newController
      a <- A.async $ wait c
      threadDelay timeout
      stop c
      threadDelay timeout
      r <- A.poll a
      r `shouldBe` Just (Right False)

    it "does not consume a stop" $ do
      c <- newController
      a <- A.async $ wait c
      b <- A.async $ wait c
      stop c
      threadDelay timeout
      r <- A.poll a
      r `shouldBe` Just (Right False)
      r' <- A.poll b
      r' `shouldBe` Just (Right False)

  describe "Controller.isStopped" $ do
    it "returns False on empty or triggered controllers" $ do
      c <- newController
      s <- isStopped c
      s `shouldBe` False
      void $ isStopped c
      s' <- isStopped c
      s' `shouldBe` False

    it "returns True on stopped controllers" $ do
      c <- newController
      stop c
      s <- isStopped c
      s `shouldBe` True

    it "persists a trigger" $ do
      c <- newController
      void $ trigger c
      void $ isStopped c
      a <- A.async $ wait c
      threadDelay timeout
      r <- A.poll a
      r `shouldBe` Just (Right True)

    it "persists a stop" $ do
      c <- newController
      stop c
      void $ isStopped c
      a <- A.async $ wait c
      threadDelay timeout
      r <- A.poll a
      r `shouldBe` Just (Right False)

    it "persists an empty" $ do
      c <- newController
      void $ isStopped c
      a <- A.async $ wait c
      threadDelay timeout
      r <- A.poll a
      r `shouldBe` Nothing

  describe "Controller.ticker" $ do
    it "stops on a stop" $ do
      c <- newController
      a <- A.async $ ticker c (timeout `div` 4)
      threadDelay timeout
      stop c
      threadDelay timeout
      r <- A.poll a
      r `shouldBe` Just (Right ())

    it "should cause triggers periodically" $ do
      c <- newController
      a <- A.async $ ticker c (timeout `div` 4)
      threadDelay timeout
      v <- wait c
      v `shouldBe` True
      threadDelay timeout
      v' <- wait c
      v' `shouldBe` True
      A.cancel a
