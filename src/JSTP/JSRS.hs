{-# LANGUAGE FlexibleInstances #-}
module JSTP.JSRS where

import qualified JSTP.ShowSettings as SS
  ( ArrayDecorator
  , arrayDecorator
  , objectDecorator
  , fieldNameValueSeparator
  , decorateArray
  , hasEscapeChars
  )

import Control.DeepSeq
import Data.List (intercalate)
import qualified Data.LinkedHashMap as M

import Text.Printf

data JValue = JObj JObject
            | JArray [JValue]
            | JDouble Double
            | JInt Int
            | JString String
            | JBool Bool
            | JUndefined
            | JNull
  deriving (Eq)

type JField = (Fieldname, JValue)

instance Show JValue where
    show JUndefined    = "undefined"
    show JNull         = "null"
    show (JBool b)     = if b then "true" else "false"
    show (JDouble num) = show num
    show (JInt num)    = show num
    show (JString str) = concat ["\"", str >>= escape, "\""]
      where escape '"' = "\\\""
            escape x   = [x]
    show (JArray arr)  = SS.decorateArray SS.arrayDecorator show arr
    show (JObj obj)    = show obj

instance Show JObject where
    show obj = SS.decorateArray SS.objectDecorator 
               (\(fieldname, jval) -> concat 
                       [ transform fieldname
                       , ":"
                       , show jval
                       ]
               ) $ takeFields obj
      where transform fieldname
              | SS.hasEscapeChars fieldname = concat ["'", fieldname, "'"]
              | otherwise                   = fieldname

type Fieldname = String

newtype JObject = JObject {
  fieldsMap :: M.LinkedHashMap Fieldname JValue
} deriving (Eq)

instance NFData JObject where
    rnf (JObject obj) = rnf obj

instance NFData JValue where
    rnf (JObj obj)    = rnf obj
    rnf (JArray arr)  = rnf arr
    rnf (JDouble num) = rnf num
    rnf (JInt num)    = rnf num
    rnf (JString str) = rnf str
    rnf (JBool bool)  = rnf bool
    rnf JUndefined    = ()
    rnf JNull         = ()

takeValue :: JObject -> Fieldname -> Maybe JValue
takeValue (JObject fields) str = M.lookup str fields

takeField :: JObject -> Fieldname -> Maybe JField
takeField obj fieldname = fmap ((,) fieldname) 
                            . M.lookup fieldname 
                            $ fieldsMap obj

takeFields :: JObject -> [JField]
takeFields = M.toList . fieldsMap

fromJString :: JValue -> Maybe String
fromJString (JString str) = Just str
fromJString _ = Nothing

fromJArray :: JValue -> Maybe [JValue]
fromJArray (JArray arr) = Just arr
fromJArray _ = Nothing

fromList :: [JField] -> JObject
fromList = JObject . M.fromList

takeValueWith :: JObject -> String -> (JValue -> a) -> Maybe a
takeValueWith obj str func = func <$> takeValue obj str

type Hash = M.LinkedHashMap String
modifyJObject :: (Hash JValue -> Hash JValue) -> JObject -> JObject
modifyJObject f = JObject . f. fieldsMap

insertField :: JField -> JObject -> JObject
insertField (fieldname, jval) = modifyJObject $ M.insert fieldname jval
