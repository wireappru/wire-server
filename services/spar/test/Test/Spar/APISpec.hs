module Test.Spar.APISpec where

import Test.Hspec
import Spar.API ()


spec :: Spec
spec = do
  describe "onSuccess" $ do
    context "new user" $ do
      it "stores user on cassandra" $ do
        False `shouldBe` True

      -- ...

    context "known user" $ do
      it "does not change cassandra state" $ do
        False `shouldBe` True

    -- ...
