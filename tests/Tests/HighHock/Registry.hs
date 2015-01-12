module Tests.HighHock.Registry (registryTests) where

import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Test.Hspec
import Control.Monad (void, forM, forM_)
import qualified Control.Concurrent.Async as A
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException)
import Data.Maybe (isJust)

import Network.HighHock.Registry
import qualified Network.HighHock.Controller as C

import Tests.HighHock.Inst ()

registryTests = do
  describe "insertContainer" $ do
    it "will add a missing container" $ do
      reg <- insertContainer newRegistry (\_ -> return ()) "id"
      M.lookup "id" reg `shouldSatisfy` isJust

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
