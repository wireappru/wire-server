module Spar.API where

import SAML2.WebSSO
import Spar.Options

runServer :: Opts -> IO ()
runServer = error . show


onSuccess :: SPHandler m => UserId -> m Void
onSuccess = undefined
