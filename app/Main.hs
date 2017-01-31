{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Conduit
import qualified Data.Conduit.Combinators as CC
import Control.Monad.Trans.Resource

import JSTP.Package

main :: IO ()
main =
  {-writeFile "bigFile" . take  (2^20) $ cycle "   { a: [1,2,3]} \0 { b : [5,2]} \0   2 }  \0  " -}
  runResourceT $ runConduit $ fuse (CC.sourceFile "bigFile") $ fuse packageParser CC.print

main2 :: IO ()
main2 = undefined --makeClient ""
