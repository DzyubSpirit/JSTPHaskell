module JSTP.Parser( JSTP.Parser.parse
                  ) where

import Text.ParserCombinators.Parsec as P
import Numeric

import JSTP.JSRS

parse :: String -> Either ParseError JValue
parse = P.parse pValue "jsrs parser"

pValue, pBool, pObject, pArray, pNumber, pUndefined, pNull :: Parser JValue
pBool = JBool <$>  (  True  <$ string "true"
                  <|> False <$ string "false"
                   ) <?> "Wrong boolean format"

pValue = spaces *> choice [pObject, pArray, pNumber, pBool, pNumber, pUndefined, pNull]
      <?> "Wrong value format"

pArray  = JArray   <$> pSeries '[' ']' pValue
pObject = (JObj . fromList <$>) . pSeries '{' '}' 
        $ (,) <$> (spaces *> pName)
              <*> (spaces *> char ':' *> spaces *> pValue)

pNumber = do
  s <- getInput
  case readSigned readFloat s of
    (n, s'):_ -> JNumber n <$ setInput s'
    _         -> fail "Wrong number format"

pString = JString <$> pString'
       
pUndefined = JUndefined <$ string "undefined"
pNull = JNull <$ string "null"

pName =  many alphaNum <|> pString' <?> "Wrong name format"

pString' :: Parser String
pString' =  between' ('"', '\'')
        <|> between' ('\'', '"')
        <?> "Wrong string format"
  where between' (outQ, inQ) = between (char outQ) (char outQ) (many $ jchar inQ)
        jchar quote =  char '\\' *> (pEscape quote <|> pUnicode)
                   <|> satisfy (`notElem` ['\\', quote])
 
pEscape :: Char -> Parser Char
pEscape quote = choice (zipWith (\c r -> r <$ char c) (quote:"bnfrt\\\"/") (quote:"\b\n\f\r\t\\/"))

pUnicode :: Parser Char
pUnicode = char 'u' *> (decode <$> count 4 hexDigit)
  where decode = toEnum . fst . (!! 0) . readHex

pSeries :: Char -> Char -> Parser a -> Parser [a]
pSeries left right parser = between (char left) (spaces *> char right)
                          $ (spaces *> parser) `sepBy` (spaces *> char ',')
