import JSRSParser
import Data.Time.Clock

main :: IO ()
main = do
  let json = "{  name: 'Marcus Aurelius',  passport: 'AE127095',  birth: {    date: '1990-02-15',    place: 'Rome'  }, age: 19,titles: [1, 2, 3, 4, {name: 'Igro',value: 335.2}]}"
  readJSRS json `seq` return ()

