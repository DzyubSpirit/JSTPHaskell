import Test.QuickCheck
import Parser

import JSTP.JSRS
import JSTP.Parser

main = quickCheck
     $ forAll (resize 2 arbitrary :: Gen JValue)
     $ \x -> either (`collect` False) (property . (== x)) . parse $ show x
