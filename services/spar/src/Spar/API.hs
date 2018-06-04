{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Spar.API where

import Control.Monad.Except
import Data.Id
import GHC.Stack
import SAML2.WebSSO hiding (UserId(..))
import Servant
import Spar.Options

import qualified SAML2.WebSSO as SAML


runServer :: Opts -> IO ()
runServer = error . show


onSuccess :: (HasCallStack, MonadSpar m) => SAML.UserId -> m Void
onSuccess = undefined


class MonadSpar m where
  -- locally read user record from Cassandra
  getUser :: SAML.UserId -> m UserId

  -- create user locally and on brig
  createUser :: SAML.UserId -> m ()

  -- get session token from brig and redirect user past login process
  forwardBrigLogin :: UserId -> m Void


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
  getUser = undefined
  createUser = undefined
  forwardBrigLogin = undefined
