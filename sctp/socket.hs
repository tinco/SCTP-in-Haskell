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
import SCTP.Socket.Types
import SCTP.Socket.Utils
import Data.Word
import qualified Data.Map as Map
import Control.Concurrent.MVar
import System.Random
import Data.Time.Clock

protocolNumber = 132 -- at least I think it is..
                     -- change this to non-standard to circumvent
                     -- OS limitations wrt capturing kernel protocols

maxMessageSize = 4096 -- RFC specifies minimum of 1500

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
        let portnum = testUdpPort + 1
        return portnum -- TODO obtain portnumber

    let initMessage = makeInit (fromIntegral myVT) $ fromIntegral myPort
    let myAddr = sockAddr (address stack, fromIntegral myPort)
    let socket = makeConnectionSocket (fromIntegral myVT) associationStateVar myAddr (fromIntegral myPort)
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
            associationPeerAddress = sockaddr,
            associationSocket = socket
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
socketAcceptMessage socket address message = do
    (eventhandler socket) (OtherEvent message)
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
                when (chunkType firstChunk == cookieEchoChunkType) $ handleCookieEcho socket address message
                unless (toProcess == []) $ do
                    maybeAssociation <- getAssociation socket tag
                    case maybeAssociation of
                        Just association  ->
                            mapM_ (handleChunk socket association) toProcess
                        Nothing -> return()
  where
    getAssociation ConnectSocket{} _ = do
        return $ Just (association socket)
    getAssociation ListenSocket{} tag = do
        assocs <- readMVar (associations socket)
        return $ Map.lookup tag assocs

handleChunk socket association chunk
    | t == initAckChunkType = handleInitAck socket association $ fromChunk chunk
    | t == payloadChunkType = handlePayload socket association $ fromChunk chunk
    | t == shutdownChunkType = handleShutdown socket association $ fromChunk chunk
    | t == cookieAckChunkType = handleCookieAck socket association $ fromChunk chunk
    | otherwise = return ()--putStrLn $ "Got chunk:" ++ show chunk -- return() -- exception?
  where
    t = chunkType chunk

handleInitAck :: Socket -> Association -> Init -> IO()
handleInitAck socket association initAck = do
    registerSocket (stack socket) (socketAddress socket) newSocket
    let cookieEcho = makeCookieEcho newAssociation initAck
    let peerAddr = peerAddress socket
    socketSendMessage socket (ipAddress peerAddr, portNumber peerAddr) cookieEcho
    swapMVar (associationState association) COOKIEECHOED
    return ()
  where
    peerVT = initiateTag initAck
    newAssociation = association { associationPeerVT = peerVT}
    newSocket = socket { association = newAssociation}

handleCookieAck :: Socket -> Association -> CookieAck -> IO()
handleCookieAck socket association initAck = do
    (eventhandler socket) $ Established association

handleShutdown :: Socket -> Association -> Shutdown -> IO()
handleShutdown socket association chunk = do
    (eventhandler socket) $ Closed association

handlePayload :: Socket -> Association -> Payload -> IO()
handlePayload socket association chunk = do 
    putStrLn "handlePayload"

handleInit :: Socket -> IpAddress -> Message -> IO()
handleInit socket@ConnectSocket{} _ message = return () -- throw away init's when we're not listening
handleInit socket@ListenSocket{} address message = do
    time <- getCurrentTime
    myVT <- randomIO :: IO Int
    myTSN <- randomIO :: IO Int
    let responseMessage = makeInitResponse address message secret time myVT myTSN
    socketSendMessage socket (address, portnum) responseMessage
    return ()
  where
    secret = secretKey socket
    portnum = fromIntegral $ (sourcePortNumber.header) message

socketSendMessage :: Socket -> (IpAddress, NBSD.PortNumber) -> Message -> IO(Int)
socketSendMessage socket address message = do
    --putStrLn $ "SendMessage: " ++ (show message) ++ "To: " ++ (show address)
    NSB.sendTo (underLyingSocket $ stack socket) messageBytes (sockAddr address)
  where
    messageBytes = (BS.concat . BL.toChunks) $ serializeMessage message

handleCookieEcho :: Socket -> IpAddress -> Message -> IO()
handleCookieEcho socket@ConnectSocket{} addr message = return ()
handleCookieEcho socket@ListenSocket{} addr message = do
    when validMac $ do
        assocs <- takeMVar $ associations socket
        association' <- liftM association (newMVar ESTABLISHED)
        let newAssocs =  Map.insert myVT association' assocs
        putMVar (associations socket) newAssocs
        (eventhandler socket) $ Established association'
        socketSendMessage socket peerAddr $ cookieAckMessage association'
        return ()
  where
    cookieChunk = fromChunk $ head $ chunks message
    (cookie,rest) = deserializeCookie $ cookieEcho cookieChunk
    myVT = verificationTag $ header message
    myAddress = address $ stack socket
    myPortnum = destinationPortNumber $ header message
    secret = secretKey $ socket
    myMac = makeMac cookie (fromIntegral myVT) myAddress myPortnum secret
    validMac = myMac == (mac cookie)
    peerVT =  peerVerificationTag cookie
    peerPort = sourcePortNumber $ header message
    peerAddr = (addr, fromIntegral peerPort)
    association = (\state -> MkAssociation peerVT myVT state myPortnum (sockAddr peerAddr) socket)
    cookieAckMessage =  (\association -> Message (makeHeader association 0) [toChunk CookieAck])

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
