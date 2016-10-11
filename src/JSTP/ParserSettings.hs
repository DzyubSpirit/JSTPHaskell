module JSTP.ParserSettings 
    ( validateFieldName
    , dropWhileWhiteSpace 
    , nameValSeps
    , fieldSeps
    , valueSeps
    , whiteSpaceChars
    , slashCombos
    , hasEscapeChars
    ) where

import JSTP.Errors

whiteSpaceChars = [' ', '\t', '\n', '\r']
nameValSeps = [':']
fieldSeps = [',']
valueSeps = [',']
------
engLetters = ['a'..'z'] ++ ['A'..'Z']
otherChars = '_' : ['0'..'9'] ++ engLetters
filterForWord = repeat otherChars 
------
slashCombos = []
------
validateFieldName :: String -> WithError String
validateFieldName []  = Left "Fieldname can not be empty"
validateFieldName [x]
  | x `elem` otherChars = Right [x]
  | otherwise = fieldNameParseError
validateFieldName str
  | not $ hasEscapeChars str = Right str
  | fChar `elem` "'\"" && fChar == lChar = Right fieldname
  | otherwise = fieldNameParseError
  where fChar = head str
        fieldname = init $ tail str
        lChar = last $ tail str

hasEscapeChars :: String -> Bool
hasEscapeChars = not . all (uncurry elem) . (`zip` filterForWord) 

dropWhileWhiteSpace :: String -> String
dropWhileWhiteSpace = dropWhile (`elem` whiteSpaceChars)
