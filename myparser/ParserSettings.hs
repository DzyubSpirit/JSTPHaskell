module JSRS.ParserSettings 
    ( validateFieldName
    , dropWhileWhiteSpace 
    , nameValSeps
    , fieldSeps
    , valueSeps
    , whiteSpaceChars
    , slashCombos
    ) where

whiteSpaceChars = [' ', '\t', '\n', '\r']
nameValSeps = [':']
fieldSeps = [',']
valueSeps = [',']
------
engLetters = ['a'..'z']++['A'..'Z']
otherChars = ['0'..'9']++engLetters
filterForWord = repeat otherChars 
------
slashCombos = []
------
validateFieldName :: String -> Bool
validateFieldName [] = False
validateFieldName str = all (uncurry elem) (zip str filterForWord)
                

dropWhileWhiteSpace :: String -> String
dropWhileWhiteSpace = dropWhile (`elem` whiteSpaceChars)
