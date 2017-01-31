module Parser where

import Control.Monad (replicateM)

import JSTP.JSRS

import Test.QuickCheck
import Test.QuickCheck.Gen

instance Arbitrary JValue where
  arbitrary = sized $ \n -> oneof $ if n > 0
  then recursive ++ elementary
  else elementary
    where bool       = fmap JBool arbitrary
          null       = return JNull
          undefined  = return JUndefined
          string     = fmap JString arbitrary
          number     = oneof [fmap JDouble arbitrary, fmap JInt arbitrary]
          elementary = [number, string, bool, undefined, null]
          recursive  = [array, object]
          array      = fmap JArray $ sized vector
          object = sized $ \n -> do
            let vectorGen :: Arbitrary a => Gen [a]
                vectorGen =  arbitrary
                        >>= (`replicateM` (resize (n - 1) arbitrary))
            names  <- vectorGen
            values <- vectorGen
            return . JObj . fromList $ zip names values
