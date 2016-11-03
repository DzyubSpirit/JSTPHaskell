module Main where

import JSTP.Parser

main :: IO ()
main = do
  let jsrs = parse "{id: 0, name: 'August' }"
  print jsrs
