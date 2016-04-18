import JSRSParser
import JSRSShow
import Data.Time.Clock
import JSRS

main :: IO ()
main = do
  let json = "{  name: 'Marcus Aurelius',  passport: 'AE127095',  birth: {    date: '1990-02-15',    place: 'Rome'  }, age: 19,titles: [1, 2, 3, 4, {name: 'Igro',value: 335.2}]}"
  readJSRS json `seq` return ()
  putStrLn (case readJSRS json of
		  		Left err -> "Error: " ++ err
		  		Right app -> show app)