import Criterion.Main
import Control.DeepSeq

import JSRS.Parser
import JSRS.JSRS

json = "{  name: 'Marcus Aurelius',  passport: 'AE127095',  birth: {    date: '1990-02-15',    place: 'Rome'  }, age: 19,titles: [1, 2, 3, 4, {name: 'Igro',value: 335.2}]}"

main = defaultMain [
        bench "1000" $ nf (map readJSRS. replicate 1000) json     
    ]
