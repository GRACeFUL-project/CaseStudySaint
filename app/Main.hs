module Main where

import Network.Wai.Handler.Warp (run)

import ServantAPI

main :: IO ()
main = do
  putStrLn "Starting server on port 8081."
  run 8081 app
