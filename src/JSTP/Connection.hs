{-# LANGUAGE OverloadedStrings #-}
module JSTP.Connection where

import Network.Simple.TCP hiding (send, recv)
import Network.Socket.ByteString
import Control.Monad(when, forever)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.MVar
import qualified Data.ByteString.Char8 as B
import Data.IORef
import Data.Either
import Data.Maybe( maybe
                 , isNothing
                 , fromMaybe
                 )
import qualified Data.Map as M

import JSTP.JSRS
import JSTP.Parser
import JSTP.Package
import JSTP.Colors
import qualified JSTP.ResInfo as ResI
import qualified JSTP.ReqInfo as ReqI

separ :: B.ByteString
separ = "\0"

separLen = B.length separ

maxChunkSize :: Int
maxChunkSize = 256

data Event = Event {
  interfaceName :: InterfaceName, 
  methodName :: MethodName,
  args :: Args,
  callback :: JValue -> IO ()
}

eventCallback = Event
event a b c = Event a b c . const . return $ ()

type InterfaceName = String
type MethodName = String
type Args = [JValue]

data Connection = Connection
  { eventPool :: Chan Event
  , endSign :: MVar ()
  , localData :: TVar JObject
  }

type ConnectionTypeConfig m r = 
       (Int -> Index
       , HostName -> ServiceName -> ((Socket, SockAddr) -> m r) -> m r
       )

type AppName = String
type LocalData = TVar JObject
type ClientMethod = (String, ClientFunc)
type Interface = (String, [ClientMethod])
type Callback = JValue -> IO ()
type CallbackPool = TVar [(Int, Callback)]

type ClientFunc = Chan Event -> LocalData -> ReqI.MethodArgs -> IO ResI.CallbackResult

noArgs = withArgs . const 
withArgs = withLocalData . const
withLocalData = withSendingEvents . const
withSendingEvents = id

flipConst = flip const

data ConnectionConfig = ConnectionConfig 
  { host :: HostName
  , port :: ServiceName
  }

data ApplicationConfig = ApplicationConfig
  { appName :: AppName
  , interfaces :: [Interface]
  }

data InteractionWays = InteractionWays
  { callbackPool :: CallbackPool
  , endSignal :: MVar ()
  , eventId :: TVar Index
  , innerEventPool :: Chan Event
  }

data Index = IncIndex Int 
           | DecIndex Int  

getInt :: Index -> Int
getInt (IncIndex x) = x
getInt (DecIndex x) = x

next :: Index -> Index
next (IncIndex x) = IncIndex $! x + 1
next (DecIndex x) = DecIndex $! x - 1

sendEvent :: Connection -> Event -> IO ()
sendEvent conn event = do
  writeChan events event
  where events = eventPool conn

initInteractionWays indexFunc = do
  eventPool <- newChan
  endSign <- newEmptyMVar
  eventId <- atomically . newTVar . next . indexFunc $ 0
  callbacks <- atomically $ newTVar []
  return $ InteractionWays callbacks endSign eventId eventPool
 
clientConfigPair = (IncIndex, connect)
serverConfigPair = (DecIndex, serveWithHost)

makeClientConnection = makeConnection clientConfigPair
makeServerConnection = makeConnection serverConfigPair

makeClientConnection' isDebug = makeConnection' isDebug clientConfigPair
makeServerConnection' isDebug = makeConnection' isDebug serverConfigPair
serveWithHost hostName = serve (Host hostName)

makeConnection = makeConnection' False

makeConnection' :: Bool -> ConnectionTypeConfig IO () -> ConnectionConfig 
                -> ApplicationConfig -> IO (TVar [Connection])
makeConnection' isDebug (indexFunc, connectionFunc) 
                (ConnectionConfig host port) appConfig = do
  connections <- atomically $ newTVar []
  forkIO . connectionFunc host port $ 
    processSocket isDebug appConfig connections indexFunc
  return connections

processSocket :: Bool -> ApplicationConfig -> TVar [Connection] 
              -> (Int -> Index) -> (Socket, SockAddr) -> IO ()
processSocket isDebug appConfig connections indexFunc (socket, addr) = do
  interactionWays <- initInteractionWays indexFunc
  localData <- atomically . newTVar . fromList $ []
  let newConnection = Connection (innerEventPool interactionWays) 
                                 (endSignal interactionWays) localData
      packageToSend = Package 0 . ReqInfo . ReqI.Handshake (appName appConfig) $ Nothing
  atomically $ modifyTVar connections (newConnection:)
  case indexFunc 0 of
    IncIndex _ -> do
      atomically $ modifyTVar (callbackPool interactionWays)
                              ((0, const $ return ()):)
      printPackage isDebug blue packageToSend
      sendPackDel socket packageToSend
    _ -> return ()
  fakeId <- forkIO $ return ()
  reader <- forkIO $ readLoop isDebug localData (interfaces appConfig) 
                     interactionWays socket B.empty
  eventer <- forkIO $ eventSendingLoop isDebug socket interactionWays
  takeMVar (endSignal interactionWays)
  killThread reader
  killThread eventer
  return ()

eventSendingLoop :: Bool -> Socket -> InteractionWays -> IO ()
eventSendingLoop isDebug socket interactionWays = do
    (Event interfaceName methodName args callback) <- readChan (innerEventPool interactionWays)
    let nextId = eventId interactionWays
    curInd <- atomically $ do
      curInd <- readTVar nextId
      writeTVar nextId . next $ curInd
      return curInd
    let curId = getInt curInd
        packageToSend = Package curId 
                      $ ReqInfo 
                      $ ReqI.Call interfaceName 
                                  (methodName, args)
    printPackage isDebug blue packageToSend
    sendPackDel socket packageToSend
    atomically $ modifyTVar (callbackPool interactionWays) ((curId, callback):)
    eventSendingLoop isDebug socket interactionWays

readLoop :: Bool -> TVar JObject -> [Interface] 
         -> InteractionWays -> Socket -> B.ByteString -> IO ()
readLoop isDebug localData interfaces interactionWays socket buff = do
  bytes <- recv socket maxChunkSize
  callbacks' <- readTVarIO (callbackPool interactionWays)
  let buff' = B.append buff bytes
      (chunks, rest) = fullChunks buff'
      packages = map 
        (\el -> do
          obj <- readJSRS (B.unpack el) 
          packageId <- takePackageId obj
          let toPackFunc = if isNothing $ lookup packageId callbacks'
                           then toReqPackage
                           else toResPackage
          toPackFunc obj
        ) chunks
      rightPackages = rights packages
  when (isDebug && bytes /= B.empty) $ do
    putStrLn (green "Chunks:")
    print chunks
    putStrLn (green "Packages:")
    print packages
  mapM_ ( performPackage isDebug localData interfaces interactionWays 
                         socket
        ) rightPackages
  when (bytes == B.empty) $ threadDelay 100
  readLoop isDebug localData interfaces interactionWays socket rest

performPackage :: Bool -> TVar JObject -> [Interface] -> InteractionWays
               -> Socket -> Package -> IO ()
performPackage isDebug localData interfaces interactionWays socket (Package id info) = case info of
  ResInfo info' -> case info' of
    ResI.Callback (Right jval) -> performCallback jval
    ResI.Callback (Left err) -> putStrLn "Callback error:"
                             >> print err
    ResI.Handshake (Right jstr) -> performCallback $ JString jstr
    ResI.Handshake (Left err) -> putStrLn "Handshake error:"
                              >> print err
    where performCallback jval = do
            callbacks' <- getCallbacks
            maybe (return ()) ($ jval) $ lookup id callbacks' 
            
  ReqInfo info' -> case info' of
    ReqI.Handshake appName _ -> do
      printPackage isDebug blue packageToSend
      sendPackDel socket packageToSend
      return ()
      where packageToSend =
              Package id . ResInfo . ResI.Handshake $
                Right "sessionHash"
    ReqI.Inspect interfaceName -> do
      when isDebug $ do
        putStrLn (blue "Sent package:")
        print packageToSend
      sendPackDel socket packageToSend
      return ()
      where packageToSend = 
              Package id . ResInfo . ResI.Callback $
              maybe (Left (12, "Interface not found"))
                    (Right . JArray . map (JString . fst))
                    (lookup interfaceName interfaces)
    ReqI.Call interfaceName (methodName, methodArgs) ->
      ( maybe (return $ Left (12, "Interface not found")) 
              (\methods -> maybe (return $ Left (13, "Method not found"))
                                 (\f -> f (innerEventPool interactionWays) localData methodArgs)
                                 (lookup methodName methods)
              ) 
              (lookup interfaceName interfaces)
      ) >>= sendPackDel socket . Package id 
          . ResInfo . ResI.Callback

  where getCallbacks = readTVarIO $ (callbackPool interactionWays)

--sendInspectPack :: Socket -> Callback -> IO ()
--sendInspectPack socket callback = 

sendPackDel :: (Show a) => Socket -> a -> IO ()
sendPackDel socket package = do
  let bytes = B.pack (show package)
  sendDel socket bytes
  return ()

sendDel :: Socket -> B.ByteString -> IO ()
sendDel socket bytes = do
  send socket $ B.append bytes separ
  return ()

printPackage :: Bool -> (String -> String) -> Package -> IO ()
printPackage isDebug color package = when isDebug $ do
  putStrLn (color "Sent package:")
  print package
 
fullChunks :: B.ByteString -> ([B.ByteString], B.ByteString)
fullChunks str = (packageStrs, rest)
  where toTuples = iterate (B.breakSubstring separ . B.drop separLen . snd)
        packageStrTuples = tail $ toTuples ("", B.append separ str)
        withWhileFunc func = func (not . B.null . snd) packageStrTuples
        packageStrs = map fst $ withWhileFunc takeWhile
        rest = fst . head $ withWhileFunc dropWhile

