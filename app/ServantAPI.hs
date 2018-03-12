{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module ServantAPI where

import Prelude hiding (flip)

import Servant
import Servant.HTML.Lucid
import Control.Monad.IO.Class

import Fish
import Image
import Geometry
import Saint

{- Saint stuff -}
type TypeUniverse = A0 Int :+: A1 []

lib :: Library TypeUniverse
lib = Library "lists"
  [ Item "map"   $ map ::: (int --> int) --> list int --> list int
  , Item "range" $ (\a b -> [a..b]) ::: int --> int --> list int
  , Item "plus"  $ (+) ::: int --> int --> int
  ]

type Universe = A0 Float :+: A0 Image :+: A0 Point

image :: A0 Image :< t => Type t Image
image = Base (inject A0)

point :: A0 Point :< t => Type t Point
point = Base (inject A0)

float :: A0 Float :< t => Type t Float
float = Base (inject A0)

fishLib :: Library Universe
fishLib = Library "fish"
  [ -- Create images
    Item "blank"          $ blank          ::: image
  , Item "addCubicBezier" $ addCubicBezier ::: point --> point --> point --> point --> image --> image
  , Item "overlay"        $ overlay        ::: image --> image --> image

    -- Hendersons functional geometry
  , Item "flip"    $ flip    ::: image --> image
  , Item "over"    $ over    ::: image --> image --> image
  , Item "besideS" $ besideS ::: float --> float --> image --> image --> image
  , Item "beside"  $ beside  ::: image --> image --> image
  , Item "aboveS"  $ aboveS  ::: float --> float --> image --> image --> image
  , Item "above"   $ above   ::: image --> image --> image
  , Item "rot"     $ rot     ::: image --> image
  , Item "rot45"   $ rot45   ::: image --> image

    -- The fish image
  , Item "fish"    $ fish ::: image
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
