module Main where

import Network.Wai.Handler.Warp (run)

import ServantAPI

main :: IO ()
main = run 8081 app
