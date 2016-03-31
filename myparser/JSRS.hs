module JSRS where

type JField = (String, JValue)
type JObject = [JField]

data JValue = JObj JObject
            | JArray [JValue]
            | JNumber Float
            | JString String
            | JBool Bool
            | JUndefined
        deriving (Show)
