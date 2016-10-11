module JSTP.Errors where

type IdError = (ErrorId, ErrorMessage)
type ErrorId = Int
type ErrorMessage = String

type WithError = Either String
type WithIdError = Either IdError

fieldNameParseError = Left "Fieldname contains restricted characters" 
