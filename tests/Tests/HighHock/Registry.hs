module Tests.HighHock.Registry (registryTests) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Test.Hspec
import Control.Monad (void, forM, forM_)
import qualified Control.Concurrent.Async as A
import Control.Concurrent (threadDelay, killThread)
import Control.Exception (SomeException)
import Data.Maybe (isJust)

import Network.HighHock.Registry
import qualified Network.HighHock.Controller as C

import Tests.HighHock.Inst ()

sleep :: a -> IO ()
sleep _ = threadDelay $ 10 ^ 6

mkReg :: [T.Text] -> IO Registry
mkReg (x:xs) = mkReg xs >>= insertContainer sleep x
mkReg [] = return newRegistry

registryTests = do
  describe "Registry.insertContainer" $ do
    it "will add a missing container" $ do
      reg <- insertContainer sleep "id" newRegistry
      M.lookup "id" reg `shouldSatisfy` isJust
      M.size reg `shouldBe` 1

    it "will run the given action" $ do
      let act c = (threadDelay $ 10 ^ 5) >> C.stop c
      reg <- insertContainer act "id" newRegistry
      a <- A.async $ C.wait (reg M.! "id")
      threadDelay $ 10 ^ 4
      r <- A.poll a
      r `shouldBe` Nothing
      threadDelay $ 10 ^ 5
      r' <- A.poll a
      r' `shouldBe` Just (Right False)

    it "will stop overwritten entries" $ do
      reg <- insertContainer sleep "id" newRegistry
      reg' <- insertContainer sleep "id" reg
      s <- C.isStopped $ reg M.! "id"
      s `shouldBe` True
      s' <- C.isStopped $ reg' M.! "id"
      s' `shouldBe` False

    it "will handle exceptions in the action" $ do
      let act _ = (threadDelay $ 10 ^ 2) >>= fail "foo"
      reg <- insertContainer act "id" newRegistry
      threadDelay $ 10 ^ 4
      s <- C.isStopped $ reg M.! "id"
      s `shouldBe` True

  describe "Registry.removeContainer" $ do
    it "will ignore missing entries" $ do
      reg <- insertContainer sleep "id" newRegistry
      r' <- removeContainer "asd" reg
      M.size r' `shouldBe` 1

    it "will remove entries" $ do
      reg <- insertContainer sleep "id" newRegistry
      r' <- removeContainer "id" reg
      M.size r' `shouldBe` 0

    it "will stop removed entries" $ do
      reg <- insertContainer sleep "id" newRegistry
      void $ removeContainer "id" reg
      s <- C.isStopped $ reg M.! "id"
      s `shouldBe` True

  describe "Registry.removeMissingContainers" $ do
    it "will remove missing containers" $ do
      reg <- mkReg ["asd", "bsd"]
      reg' <- removeMissingContainers ["asd", "nsd"] reg
      M.keys reg' `shouldBe` ["asd"]

  describe "Registry.insertMissingContainers" $ do
    it "will add missing containers" $ do
      reg <- mkReg ["asd", "bsd"]
      reg' <- insertMissingContainers (\_ _ -> threadDelay $ 10 ^ 6) ["asd", "nsd"] reg
      M.keys reg' `shouldMatchList` ["asd", "bsd", "nsd"]

  describe "Registry.removeStoppedContainers" $ do
    it "will remove naturally containers" $ do
      let act _ = threadDelay $ 10 ^ 2
      reg <- insertContainer act "id" newRegistry
      threadDelay $ 10 ^ 4
      r' <- removeStoppedContainers reg
      M.keys r' `shouldMatchList` []

    it "will remove containers with stopped controllers" $ do
      reg <- mkReg ["asd"]
      C.stop $ reg M.! "asd"
      r' <- removeStoppedContainers reg
      M.keys r' `shouldMatchList` []

    it "will not remove live containers" $ do
      reg <- mkReg ["asd"]
      r' <- removeStoppedContainers reg
      M.keys r' `shouldMatchList` ["asd"]

    it "will remove thrown containers" $ do
      let act _ = (threadDelay $ 10 ^ 2) >>= fail "foo"
      reg <- insertContainer act "id" newRegistry
      threadDelay $ 10 ^ 4
      r' <- removeStoppedContainers reg
      M.keys r' `shouldMatchList` []
