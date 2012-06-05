module SCTP.Socket where
import Network.Socket (HostAddress, HostAddress6)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Network.Socket.ByteString.Lazy as NSBL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Network.BSD as NBSD
import Control.Monad
import Control.Concurrent
import Debug.Trace
import SCTP.Types
import SCTP.Utils
import Data.Word
import qualified Data.Map as Map
import Control.Concurrent.MVar
import System.Random
import Data.Time.Clock

protocolNumber = 132 -- at least I think it is..
                     -- change this to non-standard to circumvent
                     -- OS limitations wrt capturing kernel protocols

maxMessageSize = 4096 -- RFC specifies minimum of 1500

data SCTP = MkSCTP {
    underLyingSocket :: NS.Socket,
    address :: IpAddress,
    instances :: MVar (Map.Map (IpAddress, PortNum) Socket)
}

data Socket =  
  -- Socket is an instance of SCTP
  ListenSocket {
      associations :: MVar (Map.Map VerificationTag Association),
      secretKey :: BS.ByteString,
      stack :: SCTP,
      eventhandler :: (Event -> IO())
  } |
  ConnectSocket {
      association :: Association,
      socketVerificationTag :: VerificationTag,
      socketState :: SocketState,
      eventhandler :: (Event -> IO()),
      stack :: SCTP,
      peerAddress :: NS.SockAddr,
      socketAddress :: NS.SockAddr
  }

instance Show Socket where
  show ConnectSocket {} = "ConnectSocket"
  show ListenSocket {} = "ListenSocket"

data Event = Event {
    eventMessage :: Message
}

data SocketState = CONNECTING | CONNECTED | CLOSED

-- Transmission Control Block
data Association = MkAssociation {
    associationPeerVT :: VerificationTag,
    associationVT :: VerificationTag,
    associationState :: MVar AssociationState,
    associationPort :: PortNum,
    associationPeerPort :: PortNum
}

data AssociationState = COOKIEWAIT | COOKIEECHOED | ESTABLISHED |
                        SHUTDOWNPENDING | SHUTDOWNSENT | SHUTDOWNRECEIVED |
                        SHUTDOWNACKSENT


ipAddress :: NS.SockAddr -> IpAddress
ipAddress (NS.SockAddrInet port host) = IPv4 host
ipAddress (NS.SockAddrInet6 port flow host scope) = IPv6 host

portNumber :: NS.SockAddr -> NS.PortNumber
portNumber (NS.SockAddrInet port host) = port
portNumber (NS.SockAddrInet6 port flow host scope) = port

sockAddr :: (IpAddress, NS.PortNumber) -> NS.SockAddr
sockAddr (IPv4 host, port) = NS.SockAddrInet port host
sockAddr (IPv6 host, port) = NS.SockAddrInet6 port undefined host undefined

{- Get the default local server address -}
localServerAddress = do
    host <- NBSD.getHostByName "localhost"
    return $ NBSD.hostAddress host

testUdpPort = 54312
testUdpAddress = do
    localhost <- localServerAddress -- default serveraddress for localhost
    return (NS.SockAddrInet testUdpPort localhost)

{- Create an udp socket and use that as the raw socket backend -}
start_on_udp :: NS.SockAddr -> IO (SCTP)
start_on_udp address =
    do
        sock <- NS.socket NS.AF_INET NS.Datagram NS.defaultProtocol
        NS.bindSocket sock address
        connections <- newMVar Map.empty
        let stack = MkSCTP sock (ipAddress address) connections
        thread <- forkIO (stackLoop stack)
        return stack

{- This loops on the underlying socket receiving messages and dispatching them
 - to registered sockets. -}
stackLoop :: SCTP -> IO ()
stackLoop stack = forever $ do
    (bytes, peerAddress) <- NSB.recvFrom (underLyingSocket stack) maxMessageSize
    let message = deserializeMessage bytes
    let h = header message
    let destination = (address stack, destinationPortNumber h)
    sockets <- readMVar (instances stack)
    case Map.lookup destination sockets of
        Just socket -> socketAcceptMessage socket (ipAddress peerAddress) message
        Nothing -> return ()

{- Listen on Socket -}
listen :: SCTP -> NS.SockAddr -> (Event -> IO()) -> IO (Socket)
listen stack sockaddr eventhandler = do
    associations <- newMVar Map.empty
    keyValues <- replicateM 4 (randomIO :: IO(Int))
    let secretKey = BS.pack $ map fromIntegral keyValues
    let socket = ListenSocket associations secretKey stack eventhandler
    registerSocket stack sockaddr socket
    return socket

{- Connect -}
connect :: SCTP -> NS.SockAddr -> (Event -> IO()) -> IO (Socket)
connect stack sockaddr eventhandler = do
    associations <- newMVar Map.empty
    keyValues <- replicateM 4 (randomIO :: IO(Int))
    myVT <- randomIO :: IO Int
    associationStateVar <- newMVar COOKIEWAIT 
    myPort <- do 
        let portnum = 57498
        return portnum -- TODO obtain portnumber

    let initMessage = makeInit (fromIntegral myVT) myPort
    let myAddr = sockAddr (address stack, fromIntegral myPort)
    let socket = makeConnectionSocket (fromIntegral myVT) associationStateVar myAddr myPort
    registerSocket stack myAddr socket
    socketSendMessage socket (peerAddr) initMessage
    return socket
  where
    peerAddr = (ipAddress sockaddr, portNumber sockaddr) 
    makeInit myVT myPort = message
      where
        init = Init {
            initType = initChunkType,
            initLength = fromIntegral initFixedLength,
            initiateTag = myVT,
            advertisedReceiverWindowCredit = 0,
            numberOfOutboundStreams = 1,
            numberOfInboundStreams = 0,
            initialTSN  = myVT,
            parameters = []
        }

        header = CommonHeader myPort (fromIntegral.portNumber $ sockaddr)  0 0
        message = Message header [toChunk init]
    makeConnectionSocket myVT associationStateVar myAddr myPort = socket
      where
        association = MkAssociation {
            associationPeerVT = 0,
            associationVT = myVT,
            associationState = associationStateVar,
            associationPort = myPort,
            associationPeerPort = fromIntegral $ portNumber sockaddr
        }

        socket = ConnectSocket {
          association = association,
          socketVerificationTag = myVT,
          socketState = CONNECTING,
          eventhandler = eventhandler,
          stack = stack,
          peerAddress = sockaddr,
          socketAddress = myAddr
        }

registerSocket :: SCTP -> NS.SockAddr -> Socket -> IO()
registerSocket stack addr socket =
    -- TODO simply overrides existing sockets, is this what we want?
    modifyMVar_ (instances stack) (return . Map.insert (ipAddress addr, fromIntegral(portNumber addr)) socket)

socketAcceptMessage :: Socket -> IpAddress -> Message -> IO()
socketAcceptMessage socket address message =
    -- Drop packet if verifyChecksum fails
    when (verifyChecksum message) $ do
        let tag = verificationTag $ header message
        if tag == 0 -- verification tag is 0, so message MUST be INIT
            then handleInit socket address message
            else do
                let allChunks@(firstChunk : restChunks) = chunks message
                let toProcess
                        | chunkType firstChunk == cookieEchoChunkType = restChunks
                        | otherwise = allChunks
                when (chunkType firstChunk == cookieEchoChunkType) $ handleCookieEcho message
                unless (toProcess == []) $ do
                   -- We got a valid message
                    (eventhandler socket) (Event message)
                    -- dispatch chunks to association
                    dispatch socket tag toProcess
  where
    dispatch ConnectSocket{} _ chunks = do
        mapM_ (handleChunk socket (association socket)) chunks
    dispatch ListenSocket{} tag chunks = do
        associations <- readMVar (associations socket)
        case Map.lookup tag associations of
            Just association -> mapM_ (handleChunk socket association) chunks
            Nothing -> return () -- handle OOTB cases

handleChunk socket association chunk
    | t == initAckChunkType = handleInitAck socket association $ fromChunk chunk
    | t == payloadChunkType = handlePayload socket association $ fromChunk chunk
    | t == shutdownChunkType = handleShutdown socket association $ fromChunk chunk
    | otherwise = putStrLn $ "Got chunk:" ++ show chunk -- return() -- exception?
  where
    t = chunkType chunk

makeHeader association check = CommonHeader {
    sourcePortNumber = associationPort association,
    destinationPortNumber = associationPeerPort association,
    verificationTag = associationPeerVT association,
    checksum = check
}

handleInitAck :: Socket -> Association -> Init -> IO()
handleInitAck socket association initAck = do
    registerSocket (stack socket) (socketAddress socket) newSocket
    let cookieEcho = makeCookieEcho association initAck
    let peerAddr = peerAddress socket
    -- putStrLn $ show initAck
    socketSendMessage socket (ipAddress peerAddr, portNumber peerAddr) cookieEcho
    swapMVar (associationState association) COOKIEECHOED
    return ()
  where
    peerVT = initiateTag initAck
    newAssociation = association { associationPeerVT = peerVT}
    newSocket = socket { association = newAssociation}

makeCookieEcho association init = Message (makeHeader association check) [toChunk echo]
  where
    cookie = head $ parameters init
    echo = CookieEcho {
        cookieEcho = serializeParameter cookie
    }
    check = 0

handleShutdown :: Socket -> Association -> Shutdown -> IO()
handleShutdown association chunk =
    undefined

handlePayload :: Socket -> Association -> Payload -> IO()
handlePayload association chunk =
    undefined

handleInit :: Socket -> IpAddress -> Message -> IO()
handleInit socket@ConnectSocket{} _ message = return () -- throw away init's when we're not listening
handleInit socket@ListenSocket{} address message = do
    time <- getCurrentTime
    myVT <- randomIO :: IO Int
    myTSN <- randomIO :: IO Int
    let responseMessage = makeInitResponse address message secret time myVT myTSN
    (eventhandler socket) (Event message) -- TODO trigger handleInit event
    socketSendMessage socket (address, portnum) responseMessage
    return ()
  where
    secret = secretKey socket
    portnum = fromIntegral $ (destinationPortNumber.header) message

makeInitResponse address message secret time myVT myTSN =
    Message newHeader [toChunk initAck]
  where
    portnum = destinationPortNumber mHeader
    initChunk = (fromChunk $ head $ chunks message) :: Init
    mHeader = header message
    now = timestamp time
    peerVT = initiateTag initChunk
    cookie = Cookie now peerVT
     (advertisedReceiverWindowCredit initChunk)
     (numberOfOutboundStreams initChunk)
     (numberOfInboundStreams initChunk)
     (fromIntegral myTSN)
     BS.empty

    signedCookie = cookie { mac = makeMac cookie (fromIntegral myVT) address portnum secret }

    newHeader = CommonHeader {
        sourcePortNumber = destinationPortNumber mHeader,
        destinationPortNumber = sourcePortNumber mHeader,
        verificationTag = peerVT,
        checksum = 0
    }

    initAck = Init {
        initType = initAckChunkType,
        initLength = sum $ map fromIntegral [initFixedLength, cookieLength] ,
        initiateTag = fromIntegral myVT,
        advertisedReceiverWindowCredit = advertisedReceiverWindowCredit initChunk, -- TODO be smart
        numberOfOutboundStreams = 1,
        numberOfInboundStreams = 1,
        initialTSN  = fromIntegral myTSN,
        parameters = [Parameter cookieType (fromIntegral cookieLength) (serializeCookie signedCookie)]
    }


socketSendMessage :: Socket -> (IpAddress, NBSD.PortNumber) -> Message -> IO(Int)
socketSendMessage socket address message = do
    NSB.sendTo (underLyingSocket $ stack socket) messageBytes (sockAddr address)
  where
    messageBytes = (BS.concat . BL.toChunks) $ serializeMessage message

handleCookieEcho message =
    putStrLn "Got cookie Echo"

sendToSocket :: IO()
sendToSocket = do
    sock <- NS.socket NS.AF_INET NS.Datagram NS.defaultProtocol
    localhost <- localServerAddress
    NS.bindSocket sock (NS.SockAddrInet (testUdpPort + 1) localhost)
    NSB.sendTo sock bytes (NS.SockAddrInet testUdpPort localhost)
    return ()
    where
        header = CommonHeader 1 2 3 4
        chunk = Chunk 1 1 3 (BL.pack [1,1,1])
        bytes = BS.concat $ map (BS.concat . BL.toChunks)  [serializeCommonHeader header, serializeChunk chunk]
