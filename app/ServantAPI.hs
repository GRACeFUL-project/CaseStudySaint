{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

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

type Universe = A0 Int :+: A0 Image :+: A0 Point

image :: A0 Image :< t => AnnTypeRep t Image
image = Base (inject A0)

point :: A0 Point :< t => AnnTypeRep t Point
point = Base (inject A0)

scale' i = scale (fromIntegral i)

fishLib :: Library Universe
fishLib = Library "fish"
  [ -- Create images
    Item "blank"          $ blank          ::: image
  , Item "addCubicBezier" $ addCubicBezier ::: point --> point --> point --> point --> image --> image
  , Item "overlay"        $ overlay        ::: image --> image --> image
  , Item "scale"          $ scale'         ::: int --> image --> image

    -- Hendersons functional geometry
  , Item "flip"    $ flip    ::: image --> image
  , Item "over"    $ over    ::: image --> image --> image
  , Item "besideS" $ besideS ::: int   --> int   --> image --> image --> image
  , Item "beside"  $ beside  ::: image --> image --> image
  , Item "aboveS"  $ aboveS  ::: int   --> int   --> image --> image --> image
  , Item "above"   $ above   ::: image --> image --> image
  , Item "rot"     $ rot     ::: image --> image
  , Item "rot45"   $ rot45   ::: image --> image
  , Item "natrec"  $ natrec  ::: Tag "Recursion over natural numbers"
                               $ image --> Tag "The step function" (int --> image --> image) --> int --> image

    -- The fish base image
  , Item "fish"    $ fish ::: image
  ]

runFish :: String -> IO Value
runFish s = case run image fishLib s of
  Left err  -> return $ object ["error" .= T.pack ("Error: " ++ err)]
  Right img -> do
    writeImage "test.png" img  -- for testing purposes
    return $ object ["img" .= decodeUtf8 (encodeImage img)]

describe :: String -> Value
describe x = case [tv | Item y tv <- items fishLib, x == y] of
  [tv] -> object ["result" .= T.pack (prettyTypedValue tv)]
  _    -> object ["error"  .= T.pack "Error: item not found"]
 where
  items (Library _ xs) = xs

-- Pretty printing typed values

class Render t where
  render :: Render t' => t (AnnTypeRep t') a -> String

prettyType :: Render t => AnnTypeRep t a -> String
prettyType t = case t of
  Base tr  -> render tr
  Tag s t' -> s ++ " @ " ++ prettyType t'
  a :-> b  -> "(" ++ prettyType a ++ " -> " ++ prettyType b ++ ")"

prettyTypedValue :: Render t => TypedValue t -> String
prettyTypedValue (x ::: t) = prettyType t

instance Render (A0 Bool) where
  render _ = "Bool"

instance Render (A0 Int) where
  render _ = "Int"

instance Render (A0 Image) where
  render _ = "Image"

instance Render (A0 Point) where
  render _ = "Point"

instance (Render f, Render g) => Render (CoProduct f g) where
  render (InL x) = render x
  render (InR y) = render y

-- Servant stuff

type API =  "submit"   :> ReqBody '[JSON] String :> Post '[JSON] Value
       :<|> "describe" :> Capture "id" String    :> Get  '[JSON] Value

server :: Server API
server = submit :<|> (return . describe)

submit :: String -> Handler Value
submit s = do
  liftIO . putStrLn $ "Running program:\n" ++ show s
  liftIO (runFish s)

api :: Proxy API
api = Proxy

app :: Application
app = serve api server
