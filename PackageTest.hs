module PackageTest where

import JSRS.Parser
import JSRS.Package

main = do
  let objs = [ "{callback:[15],error:[12,'Interface not found']}"
             , "{callback:[42],ok:['method1','method2']}"
             , "{handshake:[0],error:[11,'Authentication failed']}"
             , "{handshake:[0],ok:'PrivateCloud'}"
             ]
  mapM_ (print . fmap toJSTPPackage . readJSRS) objs
