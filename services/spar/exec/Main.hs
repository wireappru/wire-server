module Main where

import Control.Concurrent
import Control.Monad

main :: IO ()
main = forever $ print "." >> threadDelay 1000000
