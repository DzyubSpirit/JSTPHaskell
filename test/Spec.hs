import Test.QuickCheck
import Parser

import Data.Text (pack)

import JSTP.JSRS
import JSTP.Parser

main = quickCheck
     $ forAll (resize 2 arbitrary :: Gen JValue)
     $ \x -> (== Right x) . parse . pack $ show x
