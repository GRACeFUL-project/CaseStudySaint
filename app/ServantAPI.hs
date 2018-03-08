{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module ServantAPI where

import Servant

type Resp a = Headers '[] a

type API = "submit" :> Capture "program" String :> Post '[String] (Resp String)
