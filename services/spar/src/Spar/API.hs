{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Spar.API where

import Control.Monad.Except
import Control.Monad.Reader
import Data.Id
import GHC.Stack
import SAML2.WebSSO hiding (UserId(..))
import Servant
import Spar.Options

import qualified SAML2.WebSSO as SAML
import qualified System.Logger as Log


runServer :: Opts -> IO ()
runServer = error . show


onSuccess :: (HasCallStack, MonadSpar m) => SAML.UserId -> m Void
onSuccess uid = forwardBrigLogin =<< maybe (createUser uid) pure =<< getUser uid


class Monad m => MonadSpar m where
  -- locally read user record from Cassandra
  getUser :: SAML.UserId -> m (Maybe UserId)

  -- create user locally and on brig
  createUser :: SAML.UserId -> m UserId

  -- get session token from brig and redirect user past login process
  forwardBrigLogin :: UserId -> m Void


newtype Spar a = Spar { fromSpar :: ReaderT (Opts, Log.Logger) IO a }
  deriving (Functor, Applicative, Monad, MonadReader (Opts, Log.Logger))

instance HasConfig Spar where
  getConfig = asks (saml . fst)

instance SP Spar where
  logger level msg = asks snd >>= \logger -> Spar $ Log.log logger level' (Log.msg msg)
    where
      level' = case level of
        SILENT   -> Log.Fatal
        CRITICAL -> Log.Fatal
        ERROR    -> Log.Error
        WARN     -> Log.Warn
        INFO     -> Log.Info
        DEBUG    -> Log.Debug

  createUUID = Spar createUUIDIO
  getNow = Spar getNowIO

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
