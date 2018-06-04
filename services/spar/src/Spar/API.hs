{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Spar.API where

import Control.Monad.Except
import GHC.Stack
import SAML2.WebSSO
import Servant
import Spar.Options


runServer :: Opts -> IO ()
runServer = error . show


onSuccess :: (HasCallStack, MonadSpar m) => UserId -> m Void
onSuccess = undefined


class MonadSpar m where
  createUser :: UserId -> m ()
  forwardBrigLogin :: m Void


newtype Spar a = Spar (IO a)
  deriving (Functor, Applicative, Monad)

instance HasConfig Spar where
  getConfig = undefined

instance SP Spar where
  logger = undefined
  createUUID = undefined
  getNow = undefined

instance SPStore Spar where
  storeRequest = undefined
  checkAgainstRequest = undefined
  storeAssertion = undefined

instance MonadError ServantErr Spar where
  throwError = undefined
  catchError = undefined

instance SPHandler Spar where
  type NTCTX Spar = ()
  nt = undefined

instance MonadSpar Spar where
  createUser = undefined
  forwardBrigLogin = undefined
