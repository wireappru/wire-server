module Spar.Options where

import SAML2.WebSSO.Config (Config)

type Opts = Config  -- TODO: actually, we also need cassandra and brig coordinates from here.

cliOptsParser :: Applicative m => m Opts
cliOptsParser = undefined
