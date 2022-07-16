module Main where

import Protolude

import Network.Wai.Handler.Warp
import Web.Server

main :: IO ()
main = run 8081 app
