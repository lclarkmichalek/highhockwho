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
mkReg (x:xs) = do
  r' <- mkReg xs
  insertContainer r' sleep x
mkReg [] = return newRegistry

registryTests = do
  describe "insertContainer" $ do
    it "will add a missing container" $ do
      reg <- insertContainer newRegistry sleep "id"
      M.lookup "id" reg `shouldSatisfy` isJust
      M.size reg `shouldBe` 1

    it "will run the given action" $ do
      let act c = (threadDelay $ 10 ^ 5) >> C.stop c
      reg <- insertContainer newRegistry act "id"
      a <- A.async $ C.wait (fst $ reg M.! "id")
      threadDelay $ 10 ^ 4
      r <- A.poll a
      r `shouldBe` Nothing
      threadDelay $ 10 ^ 5
      r' <- A.poll a
      r' `shouldBe` Just (Right False)
      killThread . snd $ reg M.! "id"

    it "will overwrite existing entries" $ do
      reg <- insertContainer newRegistry sleep "id"
      reg' <- insertContainer reg sleep "id"
      snd (reg' M.! "id") `shouldSatisfy` (/= snd (reg M.! "id"))

    it "will stop overwritten entries" $ do
      reg <- insertContainer newRegistry sleep "id"
      void $ insertContainer reg sleep "id"
      s <- C.wait . fst $ reg M.! "id"
      s `shouldBe` False

  describe "removeContainer" $ do
    it "will ignore missing entries" $ do
      reg <- insertContainer newRegistry sleep "id"
      r' <- removeContainer reg "asd"
      M.size r' `shouldBe` 1

    it "will remove entries" $ do
      reg <- insertContainer newRegistry sleep "id"
      r' <- removeContainer reg "id"
      M.size r' `shouldBe` 0

    it "will stop removed entries" $ do
      reg <- insertContainer newRegistry sleep "id"
      void $ removeContainer reg "id"
      s <- C.wait . fst $ reg M.! "id"
      s `shouldBe` False

  describe "removeMissingContainers" $ do
    it "will remove missing containers" $ do
      reg <- mkReg ["asd", "bsd"]
      reg' <- removeMissingContainers reg ["asd", "nsd"]
      M.keys reg' `shouldBe` ["asd"]

  describe "insertMissingContainers" $ do
    it "will add missing containers" $ do
      reg <- mkReg ["asd", "bsd"]
      reg' <- insertMissingContainers reg (\_ _ -> threadDelay $ 10 ^ 6) ["asd", "nsd"]
      M.keys reg' `shouldMatchList` ["asd", "bsd", "nsd"]
