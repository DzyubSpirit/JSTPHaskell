module JSRSParser(readJSRS
                 ,readJValue
  ) where

import ParserSettings(validateFieldName
                     ,dropWhileWhiteSpace
                     ,nameValSeps
                     ,fieldSeps
                     ,whiteSpaceChars
                     ,slashCombos
                     ,valueSeps
                     )
import Data.List(isPrefixOf)
import Data.Maybe(fromJust)
import Data.Char(isDigit)

import Data.Time.Clock

import Control.Applicative(liftA2)

import JSRS


readJSRS :: String -> Either String JObject
readJSRS = fmap fst . readJSRSRest

readJSRSRest :: String -> Either String (JObject, String)
readJSRSRest str = case str' of
        [] -> Left "JSRS parse error: empty string"
        '{':inner -> readFields inner
        _ -> Left "Must be open figure bracket"
    where str' = dropWhileWhiteSpace str

readFields :: String -> Either String (JObject, String)
readFields str = case str' of
        '}':rest -> Right ([], rest)
        _        -> do
            (field, str'') <- readField str'
            case dropWhileWhiteSpace str'' of
                '}':rest -> Right ([field], rest)
                ch:rest  -> if elem ch fieldSeps
                            then fmap (mapFst (field:)) (readFields rest)
                            else Left "Wrong field separator"
                _        -> Left "Must be close figure bracket"
    where str' = dropWhileWhiteSpace str


readJValue :: String -> Either String JValue
readJValue str = fmap fst (readJValueRest str)

readJValueRest :: String -> Either String (JValue, String)
readJValueRest str2
    | getToken "true" /= Nothing = Right (JBool True, fromJust (getToken "true"))
    | getToken "false" /= Nothing = Right (JBool False, fromJust (getToken "false"))
    | numberStr /= [] = Right (JNumber (read numberStr), drop (length numberStr) str)
    | otherwise = case str of
        ('\'':rest) -> fmap (mapFst JString) (readString rest)
        ('{':rest)  -> fmap (mapFst JObj)    (readJSRSRest str)  
        ('[':rest)  -> fmap (mapFst JArray)  (readJValues rest)
        _ -> Left "Unknown data type"

  where str = dropWhileWhiteSpace str2
        getToken token = if token `isPrefixOf` str
          then Just (drop (length token) str)
          else Nothing
        numberStr = getNumber False str
        getNumber wasDot [] = []
        getNumber wasDot (x:str) = 
            if isDigit x
            then x:(getNumber wasDot str)
            else if x == '.'
                 then if wasDot 
                      then [] 
                      else x:(getNumber True str)
                 else []

readJValues :: String -> Either String ([JValue], String)
readJValues str = case str' of
        ']':rest -> Right ([], rest)
        _   -> do
            (value, str') <- readJValueRest str
            case dropWhileWhiteSpace str' of
                ']':rest -> Right ([value], rest)
                ch:rest -> if elem ch valueSeps
                           then fmap (mapFst (value:)) (readJValues rest)
                           else Left "Wrong value separator"
                _ -> Left "No close bracket"
    where str' = dropWhileWhiteSpace str

readString :: String -> Either String (String, String)
readString ('\'':str) = Right ("", str)
readString ('\\':x:str) = fmap (mapFst (x:)) (readString str)
readString (ch:str) = fmap (mapFst (ch:)) (readString str)
readString [] = Left "There is not close quote for string literal"

dropSeparator :: [Char] -> String -> Either String String
dropSeparator separators str = case str' of 
        (x:rest) -> if elem x separators
                    then Right rest
                    else Left "Wrong separator"
    where str' = dropWhileWhiteSpace str

dropFieldSeparator :: String -> Either String String
dropFieldSeparator = dropSeparator fieldSeps

dropNameValSeparator :: String -> Either String String
dropNameValSeparator = dropSeparator nameValSeps 

readField :: String -> Either String (JField, String)
readField str = do
        (fieldName, rest) <- readFieldName str' 
        rest' <- dropNameValSeparator rest
        (fieldVal, rest') <- readFieldValue rest'
        return ((fieldName, fieldVal), rest')
    where str' = dropWhileWhiteSpace str


readFieldName :: String -> Either String (String, String)
readFieldName str = if validateFieldName fieldName
                  then Right (fieldName, rest)
                  else Left ("Fieldname \"" ++ fieldName++"\" is invalid")
    where delChars = nameValSeps++whiteSpaceChars
          fieldName = takeWhile (`notElem` delChars) str
          rest = dropWhile (`notElem` delChars) str

readFieldValue :: String -> Either String (JValue, String)
readFieldValue = readJValueRest . dropWhileWhiteSpace

mapFst :: (a -> c) -> (a, b) -> (c, b)
mapFst fn (a, b) = (fn a, b)

addResult :: Either a (b, c) -> Either a ([b], c) -> Either a ([b], c)
addResult = liftA2 (\(x,_) -> mapFst (x:))
