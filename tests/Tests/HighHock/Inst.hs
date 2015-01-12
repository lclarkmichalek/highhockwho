module Tests.HighHock.Inst where

import qualified Network.HighHock.Controller as C

import Control.Exception (SomeException)

instance Show C.Controller where
  show c = "Controller _"

instance Eq SomeException where
  (==) a b = show a == show b
