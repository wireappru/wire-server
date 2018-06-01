module Main where

import Spar.API
import Spar.Config
import Util.Options

main :: IO ()
main = do
  let desc = "Brig - User Service"
      defaultPath = "/etc/wire/brig/conf/brig.yaml"
  options <- getOptions desc cliOptsParser defaultPath
  runServer options
