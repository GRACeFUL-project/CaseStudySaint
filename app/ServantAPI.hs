{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ServantAPI where

import Servant
import Servant.HTML.Lucid
import Control.Monad.IO.Class

import Saint

{- Saint stuff -}
type TypeUniverse = A0 Int :+: A1 []

lib :: Library TypeUniverse
lib = Library "lists"
  [ Item "map"   $ map ::: (int --> int) --> list int --> list int
  , Item "range" $ (\a b -> [a..b]) ::: int --> int --> list int
  , Item "plus"  $ (+) ::: int --> int --> int
  ]

runFun :: String -> String
runFun s = case run (list int) lib s of
  Left err  -> "Error: " ++ err
  Right lst -> show lst

{- Servant stuff -}
type API = "submit" :> ReqBody '[JSON] String :> Post '[HTML] String

server :: Server API
server = submit

submit :: String -> Handler String
submit s = do
  liftIO . putStrLn $ "Running program:\n" ++ show s
  return (runFun s)

api :: Proxy API
api = Proxy

app :: Application
app = serve api server
