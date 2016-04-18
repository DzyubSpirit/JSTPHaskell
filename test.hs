{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import Control.Applicative ((<$>), (<*>), empty)
import Data.Aeson
import Data.Monoid
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Time.Clock

main :: IO ()
main = do
  let json = " { } "
      req = decode json :: Maybe Value
  req `seq` return ()
