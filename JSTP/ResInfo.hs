module JSTP.ResInfo where

import JSTP.JSRS
import JSTP.Errors

data ResInfo = Handshake HandshakeResult
             | Callback  CallbackResult

type HandshakeResult = Either IdError String 
type ClientMethod = String
type CallbackResult = Either IdError JValue  

emptyCallbackResult = Right . JArray $ []
