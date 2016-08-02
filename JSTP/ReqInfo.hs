module JSTP.ReqInfo where

import JSTP.JSRS

data ReqInfo = Inspect InterfaceName
             | Handshake AppName (Maybe UserInfo)
             | Call InterfaceName MethodInfo

type AppName = String
type MethodInfo = (MethodName, MethodArgs)
type MethodName = String
type MethodArgs = [JValue]
type UserInfo = (UserName, SessionHash)
type InterfaceName = String
type SessionHash = String
type UserName = String
