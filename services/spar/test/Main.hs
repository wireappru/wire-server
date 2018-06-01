module Main where

import Test.Tasty

import qualified Test.Spar.Options

main :: IO ()
main = do
    defaultMain $ testGroup "Tests"
        [ Test.Spar.Options.tests
        ]
