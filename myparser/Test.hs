import Criterion.Main
import Control.DeepSeq

import JSRSParser
import JSRS

instance NFData JObject where
    rnf (JObject obj) = rnf obj

instance NFData JField where
    rnf (JField (a, b)) = rnf (rnf a, rnf b)

instance NFData JValue where
    rnf (JObj obj) = rnf obj
    rnf (JArray arr) = rnf arr
    rnf (JNumber num) = rnf num
    rnf (JString str) = rnf str
    rnf (JBool bool) = rnf bool
    rnf JUndefined = ()

json = "{  name: 'Marcus Aurelius',  passport: 'AE127095',  birth: {    date: '1990-02-15',    place: 'Rome'  }, age: 19,titles: [1, 2, 3, 4, {name: 'Igro',value: 335.2}]}"

main = defaultMain [
        bench "1000" $ nf (map readJSRS. replicate 1000) json     
    ]