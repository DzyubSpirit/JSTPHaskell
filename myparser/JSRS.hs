module JSRS where

newtype JField = JField (String, JValue)
newtype JObject = JObject [JField]

data JValue = JObj JObject
            | JArray [JValue]
            | JNumber Float
            | JString String
            | JBool Bool
            | JUndefined
