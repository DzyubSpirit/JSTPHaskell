module JSTP.Show where

import JSTP.JSRS

import qualified JSTP.ShowSettings as SS( ArrayDecorator
                                        , arrayDecorator
                                        , objectDecorator
                                        , fieldNameValueSeparator
                                        , decorateArray
                                        )

import Data.List(intercalate)

instance Show JValue where
    show JUndefined = "undefined"
    show (JBool b) = if b then "true" else "false"
    show (JString str) = show str
    show (JNumber num) = show num
    show (JArray arr) = SS.decorateArray SS.arrayDecorator arr
    show (JObj obj) = show obj

instance Show JField where
    show (JField (name, value)) = name ++ SS.fieldNameValueSeparator ++ (show value)

instance Show JObject where
    show (JObject obj) = SS.decorateArray SS.objectDecorator obj
