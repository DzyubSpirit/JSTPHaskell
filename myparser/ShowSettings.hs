module ShowSettings where

import Data.List(intercalate)

data ArrayDecorator = ArrayDecorator {
    start :: String,
    end :: String,
    separator :: String
}

decorateArray :: (Show a) => ArrayDecorator -> [a] -> String
decorateArray dec arr = (start dec) ++ arrStr ++ (end dec)
    where arrStr = intercalate (separator dec) $ map show arr

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