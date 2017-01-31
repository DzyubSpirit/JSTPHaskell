{-# LANGUAGE OverloadedStrings #-}
module JSTP.Parser ( JSTP.Parser.parse
                   , pValue
                   , pObject
                   ) where

import Data.Attoparsec.Text as P
import Data.Attoparsec.Combinator
import Data.Char (isAlphaNum, isHexDigit)
import Control.Applicative
import Numeric

import JSTP.JSRS

parse = eitherResult . P.parse pValue

pPackage = pValue <* char '\0'

pValue:: Parser JValue
pValue = skipSpace *> choice 
  [ JObj    <$> pObject
  , JArray  <$> pArray
  , JInt    <$> signed decimal
  , JDouble <$> double
  , JBool   <$> pBool
  , JString <$> pString
  , pUndefined
  , pNull
  ] <?> "javascript value"

pBool :: Parser Bool
pBool = (  True  <$ string "true"
       <|> False <$ string "false"
        ) <?> "Wrong boolean format"

pArray :: Parser [JValue]
pArray  = pSeries '[' ']' pValue

pObject :: Parser JObject
pObject = (fromList <$>) . pSeries '{' '}' 
        $ (,) <$> (skipSpace *> pName)
              <*> (skipSpace *> char ':' *> skipSpace *> pValue)

pUndefined, pNull :: Parser JValue
pUndefined = JUndefined <$ string "undefined"
pNull = JNull <$ string "null"

pName, pString :: Parser String
pName = pString <|> many1 (satisfy isAlphaNum)
pString =  between' '"'
       <|> between' '\''

between open close p = open >> p <* close
between' quote = between (char quote) (char quote) (many $ jchar quote)
jchar quote =  char '\\' *> (pEscape quote <|> pUnicode)
           <|> satisfy (`notElem` ['\\', quote])
 
pEscape :: Char -> Parser Char
pEscape quote = choice (zipWith (\c r -> r <$ char c) (quote:"bnfrt\\\"/")
                                                      (quote:"\b\n\f\r\t\\/")
                       )

pUnicode :: Parser Char
pUnicode = char 'u' *> (decode <$> count 4 (satisfy isHexDigit))
  where decode = toEnum . fst . (!! 0) . readHex

pSeries :: Char -> Char -> Parser a -> Parser [a]
pSeries left right parser = between (char left) (skipSpace *> char right)
                          $ (skipSpace *> parser) `sepBy` (skipSpace *> char ',')
