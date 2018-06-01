module Test.Spar.Options where

import Spar.Options
import Test.Tasty
import Test.Tasty.HUnit
import Util.Options


tests :: TestTree
tests = testGroup "Options Tests"
    [ testCase "parse" $ do
        result <- getOptions "desc" cliOptsParser "spar.integration.yaml"
        assertBool "contains impure exceptions" $ length (show result) > 0
    ]
