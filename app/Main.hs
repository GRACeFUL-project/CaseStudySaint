{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors

import ServantAPI

main :: IO ()
main = do
  putStrLn "Starting server on port 8081."
  run 8081 $ cors (const $ Just policy) app
 where
  policy = simpleCorsResourcePolicy { corsRequestHeaders = ["content-type"] }


-- Example call to get 't':
-- curl -H "Content-Type: application/json" --data '"scale (let fish2 = flip (rot45 fish) in let fish3 = rot (rot (rot fish2)) in over fish (over fish2 fish3))"' http://localhost:8081/submit
-- 
