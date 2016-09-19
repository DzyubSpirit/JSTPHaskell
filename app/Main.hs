module Main where

import JSTP.Parser

main :: IO ()
main = do
  let jsrs = readJSRS "{id: 0, name: 'August' }"
  print jsrs
