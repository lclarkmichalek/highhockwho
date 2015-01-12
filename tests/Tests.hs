module Main where

import Test.Hspec

import Tests.HighHock.Controller
import Tests.HighHock.Registry

main :: IO ()
main = hspec $ do
  controllerTests
  registryTests
