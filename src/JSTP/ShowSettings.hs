module JSTP.ShowSettings where

import Data.List(intercalate)
import Data.Char(isAlphaNum)

data ArrayDecorator = ArrayDecorator {
    start :: String,
    end :: String,
    separator :: String
}

decorateArray :: ArrayDecorator -> (a -> String) -> [a] -> String
decorateArray dec showFunc arr = (start dec) ++ arrStr ++ (end dec)
    where arrStr = intercalate (separator dec) $ map showFunc arr

arrayDecorator = ArrayDecorator {
    start = "[",
    end = "]",
    separator = ","
}

objectDecorator = ArrayDecorator {
    start = "{",
    end = "}",
    separator = ","    
}

fieldNameValueSeparator = ":"

hasEscapeChars = and . map isAlphaNum
