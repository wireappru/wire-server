{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}

module Test.Spar.APISpec where

import Control.Monad.State
import Spar.API
import Test.Hspec

import qualified Data.Map as Map
import qualified SAML2.WebSSO as SAML


data TestState = TestState
  { callmap  :: Map.Map String Int
  , preexist :: [SAML.UserId]
  }
  deriving (Eq, Show)

newtype TestSpar a = TestSpar { fromTestSpar :: State TestState a }
  deriving (Functor, Applicative, Monad)

instance MonadSpar TestSpar where
  getUser = undefined
  createUser = undefined
  forwardBrigLogin = undefined

run :: TestSpar a -> TestState -> TestState
run = fmap snd . runState . fromTestSpar


spec :: Spec
spec = do
  describe "onSuccess" $ do
    let uid = SAML.UserId (SAML.Issuer (SAML.opaqueNameID "wef")) (SAML.opaqueNameID "phoo")
        tste = TestState mempty []
        tstf = TestState mempty [uid]

    context "new user" $ do
      it "calls createUser" $ do
        (onSuccess uid `run` tste) `shouldSatisfy`
          \(TestState cm _) -> Map.lookup "createUser" cm == Just 1

    context "known user" $ do
      it "does not call createUser" $ do
        (onSuccess uid `run` tstf) `shouldSatisfy`
          \(TestState cm _) -> Map.lookup "createUser" cm == Nothing

    it "calls forwardBrigLogin" . forM_ [tste, tstf] $ \tst -> do
      (onSuccess uid `run` tst) `shouldSatisfy`
        \(TestState cm _) -> Map.lookup "forwardBrigLogin" cm == Just 1
