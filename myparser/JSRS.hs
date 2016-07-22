module JSRS.JSRS where

import Control.DeepSeq

type JField = (String, JValue)
newtype JObject = JObject [JField]

data JValue = JObj JObject
            | JArray [JValue]
            | JNumber Float
            | JString String
            | JBool Bool
            | JUndefined

instance Show JObject where
  show (JObject arr) = show arr

instance Show JValue where
  show JUndefined = "undefined"
  show (JBool True) = "true"
  show (JBool False) = "false"
  show (JString str) = show str
  show (JNumber num) = show num
  show (JArray arr) = show arr
  show (JObj obj) = "Obj " ++ (show obj)

instance NFData JObject where
    rnf (JObject obj) = rnf obj

instance NFData JValue where
    rnf (JObj obj) = rnf obj
    rnf (JArray arr) = rnf arr
    rnf (JNumber num) = rnf num
    rnf (JString str) = rnf str
    rnf (JBool bool) = rnf bool
    rnf JUndefined = ()

takeField :: String -> JObject -> Maybe JValue
takeField str (JObject fields) = lookup str fields

fromJString :: JValue -> Maybe String
fromJString (JString str) = Just str
fromJString _ = Nothing

takeFieldWith :: JObject -> String -> (JValue -> a) -> Maybe a
takeFieldWith obj str func = fmap func . takeField str $ obj 
