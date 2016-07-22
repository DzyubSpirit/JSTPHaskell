{-# LANGUAGE OverloadedStrings, RecordWildCards #-}

import Criterion.Main
import Control.DeepSeq
import qualified Data.ByteString.Lazy.Char8 as BL

import Data.Aeson

jsonStr = "{  name: 'Marcus Aurelius',  passport: 'AE127095',  birth: {    date: '1990-02-15',    place: 'Rome'  }, age: 19,titles: [1, 2, 3, 4, {name: 'Igro',value: 335.2}]}"

test :: Int -> BL.ByteString -> [Maybe Value]
test count =  map decode . replicate count

main = defaultMain [
        bench "1000" $ nf (test 1000000) jsonStr     
    ]

