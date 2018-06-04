module Test.Spar.APISpec where

import Spar.API ()
import Test.Hspec


spec :: Spec
spec = do
  describe "onSuccess" $ do
    context "new user" $ do
      it "calls createUser" $ do
        pending

    context "known user" $ do
      it "does not call createUser" $ do
        pending

    it "calls forwardBrigLogin" $ do
      pending
