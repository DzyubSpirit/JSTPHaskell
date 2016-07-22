module JSRS.Package where

import Data.Functor((<$>))
import Data.Monoid( First(..)
                  , mconcat
                  )
import Data.Maybe( fromJust
                 , fromMaybe
                 )

import JSRS.JSRS

data Package = Package Int PackageInfo 
  deriving (Show)

data PackageInfo = Handshake HandshakeResult
                 | Callback  CallbackResult
                 | Inspect
  deriving (Show)

type HandshakeResult = Either (ErrorID, ErrorMessage) String 
type ErrorID = Int
type ErrorMessage = String

type CallbackResult = Either (ErrorID, ErrorMessage) JValue  

toJSTPPackage :: JObject -> Either String Package
toJSTPPackage obj = fromMaybe (Left "Uknown package type") 
                  . getFirst . mconcat 
                  . map (First . takePackage) $
  [ ("handshake", fmap Handshake $ takePackageInfo fromJString)
  , ("callback" , fmap Callback  $ takePackageInfo (return . id))
  , ("inspect", return Inspect)
  ]
    where takeFirstInt (JArray ((JNumber id):_)) = Just $ round id
          takeFirstInt _ = Nothing

          takeError (JArray ((JNumber err):(JString msg):_)) = Just (round err, msg)
          takeError _ = Nothing

          takeInfo (name, err, toInfo) =
            takeFieldWith obj name $
              maybe err Right . toInfo

          takePackageInfo takeOk = fromMaybe (Left "No ok or error field")
                                 . getFirst . mconcat 
                                 . map (First . takeInfo) $
            [ ("ok"   , ok_error , fmap Right . takeOk)
            , ("error", err_error, fmap Left  . takeError)
            ]

          takePackage (name, infoFunc) = fmap (takePackage' infoFunc) $
            takeField name obj

          takePackage' infoFunc package_field = do
            package_id <- maybe id_error Right $ 
              takeFirstInt package_field
            package_info <- infoFunc
            return $ Package package_id package_info
           
          id_error = Left "Wrong packet id format"
          ok_error = Left "Wrong ok format"
          err_error = Left "Wrong error format"
