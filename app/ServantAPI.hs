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
import Data.Aeson.Types hiding (Bool, String, Parser, Options)
import qualified Data.Aeson.Types as Aeson
import qualified Data.Text as T
import Data.Text.Lazy.Encoding

import Fish
import Image
import Geometry
import Saint


type Universe = A0 Int :+: A0 Float :+: A0 Image :+: A0 Point

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
  , Item "scale"          $ scale 1000.0   ::: image --> image

    -- Hendersons functional geometry
  , Item "flip"    $ flip    ::: image --> image
  , Item "over"    $ over    ::: image --> image --> image
  , Item "besideS" $ besideS ::: int   --> int   --> image --> image --> image
  , Item "beside"  $ beside  ::: image --> image --> image
  , Item "aboveS"  $ aboveS  ::: int   --> int   --> image --> image --> image
  , Item "above"   $ above   ::: image --> image --> image
  , Item "rot"     $ rot     ::: image --> image
  , Item "rot45"   $ rot45   ::: image --> image

    -- The fish base image
  , Item "fish"    $ fish ::: image
  ]

runFish :: String -> IO Value
runFish s = case run image fishLib s of
  Left err  -> return $ Aeson.String $ T.pack $ "Error: " ++ err
  Right img -> do 
    writeImage "test.png" img  -- for testing purposes
    return $ object ["img" .= decodeUtf8 (encodeImage img)]

{- Servant stuff -}

type Resp a = Headers '[Header "Access-Control-Allow-Origin" String] a

hdr :: Handler a -> Handler (Resp a)
hdr h = h >>= return . addHeader "*"

type API = "submit" :> ReqBody '[JSON] String :> Post '[JSON] (Resp Value)

server :: Server API
server = submit

submit :: String -> Handler (Resp Value)
submit s = hdr $ do
  liftIO . putStrLn $ "Running program:\n" ++ show s
  liftIO (runFish s)

api :: Proxy API
api = Proxy

app :: Application
app = serve api server
