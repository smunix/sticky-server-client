{-# LANGUAGE TypeFamilies #-}
module Network.Internal.Socket ( HostPort(..)
                               , Client
                               , Server
                               , accept
                               , safeRecv
                               , recvWithLen
                               , safeSend
                               , SetupSocket(..)
                               , initSocketServer
                               , initSocketClient
                               ) where

import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendAll)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString as W8
import qualified Text.Read (read)
import Data.Serialize
import System.Console.Chalk
import System.Timeout
import Control.Monad
import Control.Exception
import Control.Arrow
import Foreign.Storable
import Data.Word

type BString = C8.ByteString
data HostPort = HostPort { hpHost :: HostName
                         , hpPort :: ServiceName } deriving (Show, Eq)

getHostName :: IO String
getHostName = return "localhost"

safeRecv :: Socket -> Int -> IO BString
safeRecv skt len = do
  str <- recv skt len
  let strLen = C8.length str
  if strLen < len
    then do nextStr <- safeRecv skt (len-strLen)
            return $ str `C8.append` nextStr
    else return str

safeSend :: Socket -> BString -> IO ()
safeSend = sendAll

recvWithLen :: Socket -> Int -> IO BString
recvWithLen skt len = do
  eLen <- runGet (get :: Get Word32) <$> safeRecv skt len
  case eLen of
    Left _ -> error "die"
    Right len -> do
      print len
      safeRecv skt (fromIntegral len)

data Server
data Client

server :: Server
server = undefined
client :: Client
client = undefined

class SetupSocket a where
  type InitSocketSignature a
  type LaunchSocketSignature a
  initSocket :: a -> InitSocketSignature a
  launchSocket :: a -> LaunchSocketSignature a

instance SetupSocket Server where
  type InitSocketSignature Server = (ServiceName -> IO (Socket, SockAddr))
  initSocket _ svc = do
    (svcAddr:_) <- getAddrInfo
                   (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                   Nothing
                   (Just svc)
    skt <- socket (addrFamily svcAddr) Stream defaultProtocol
    return (skt, addrAddress svcAddr)

  type LaunchSocketSignature Server = (ServiceName -> IO (Maybe (Socket, SockAddr)))
  launchSocket _ svc = do
    sktR@(skt, sktAddr) <- initSocket (undefined :: Server) svc
    let delay = 10*1000000
    attempt <- timeout delay (try (bind skt sktAddr))
    case (attempt :: Maybe (Either IOException ())) of
      Nothing -> do
        close skt
        putStrLn $ red $ "Timeout error when binding port " ++ svc
        return Nothing
      Just (Left e) -> do
        close skt
        putStrLn $ red $ "Couldn't bind port " ++ svc ++ " because " ++ show e
        return Nothing
      Just (Right ()) -> do
        listen skt 1000
        putStrLn $ green $ "Listening to " ++ svc
        return . Just $ sktR

initSocketServer :: ServiceName -> IO (Socket, SockAddr)
initSocketServer = initSocket (undefined :: Server)

launchSocketServer :: ServiceName -> IO (Maybe (Socket, SockAddr))
launchSocketServer = launchSocket (undefined :: Server)

instance SetupSocket Client where
  type InitSocketSignature Client = (HostName -> ServiceName -> IO (Socket, SockAddr))
  type LaunchSocketSignature Client = (HostName -> ServiceName -> IO (Maybe (Socket, SockAddr)))

  initSocket _ host svc = do
    (svcAddr:_) <- getAddrInfo Nothing (Just host) (Just svc)
    skt <- socket (addrFamily svcAddr) Stream defaultProtocol
    return (skt, addrAddress svcAddr)

  launchSocket _ host svc = do
    sktR@(skt, sktAddr) <- initSocket (undefined :: Client) host svc
    let delay = 5*1000000
    attempt <- timeout delay (try (connect skt sktAddr))
    case (attempt :: Maybe (Either IOException ())) of
      Nothing -> do
        close skt
        putStrLn $ red $ "Timeout error when connecting to " ++ host ++ ":" ++ svc
        return Nothing
      Just (Left e) -> do
        close skt
        putStrLn $ red $ "Couldn't connect to " ++ host ++ ":" ++ svc ++ " because " ++ show e
        return Nothing
      Just (Right ()) -> do
        putStrLn $ green $ "Connected to " ++ host ++ ":" ++ svc
        return . Just $ sktR

initSocketClient :: HostName -> ServiceName -> IO (Socket, SockAddr)
initSocketClient = initSocket (undefined :: Client)

launchSocketClient :: HostName -> ServiceName -> IO (Maybe (Socket, SockAddr))
launchSocketClient = launchSocket (undefined :: Client)
