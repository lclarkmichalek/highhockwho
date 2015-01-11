module Main where

import Test.Hspec

import Tests.HighHock.Controller

main :: IO ()
main = hspec $ do
  controllerTests
