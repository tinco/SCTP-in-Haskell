{-# LANGUAGE RecordWildCards #-}
module SCTP.Monadic where
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 (fromString)
import qualified Network.BSD as NBSD
import Control.Monad
import Control.Concurrent
import SCTP.Types
import SCTP.Utils
import SCTP.Socket.Types
import SCTP.Socket.Utils
import SCTP.Socket.Timer
import qualified Data.Map as Map
import System.Random
import Data.Time.Clock

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
connect stack peerAddr eventhandler = do
    myVT <- liftM fromIntegral (randomIO :: IO Int)
    myPort <- liftM fromIntegral $ do 
        let portnum = testUdpPort + 1
        return portnum -- TODO obtain portnumber

    let myAddr = sockAddr (address stack, fromIntegral myPort)
    associationMVar <- newEmptyMVar
    initTimer <- startTimer 10

    let socket = makeConnectionSocket stack myVT associationMVar myAddr eventhandler peerAddr
    let association' = makeAssociation socket (myVT) myPort peerAddr initTimer
    putMVar (association socket) association'
    registerSocket stack myAddr socket

    let initMessage = makeInitMessage myVT myPort peerAddr
    socketSendMessage socket (ipAddress peerAddr, portNumber peerAddr) initMessage
    registerTimer initTimer 0 (initRetransmit association' initMessage 0) (associationTimeOut association')
    return socket

{- Retransmit the init message -}
initRetransmit :: Association -> Message -> Integer -> IO ()
initRetransmit association message attempt = do
    if attempt < defaultMaxInitRetransmits
      then do
          socketSendMessage socket (ipAddress peerAddr, portNumber peerAddr) message
          registerTimer timer 0 (initRetransmit association message $ attempt+1) timeOut
          return ()
      else do
          closeConnection association
          (eventhandler socket) $ Error "Could not connect."
          return ()
  where
    peerAddr = associationPeerAddress association
    socket = associationSocket association
    timer = associationTimer association
    timeOut = associationTimeOut association

{- Close the connection -}
closeConnection :: Association -> IO()
closeConnection association = return ()

{- Send a string -}
sendString :: Association -> String -> IO()
sendString association string =
    sendData association $ fromString string
    
{- Sends some bytes -}
sendData :: Association -> BS.ByteString -> IO()
sendData association bytes = do
    socketSendMessage socket (ipAddress peerAddress, portNumber peerAddress) message
    return ()
  where
    message = Message header [toChunk payload]
    header = makeHeader association 0
    payload = Payload 0 True True True pLength tsn 0 0 0 $ BL.fromChunks [padded_bytes]
    pLength = fixedPayloadLength + (fromIntegral $ BS.length bytes)
    padded_bytes = payloadPad bytes
    peerAddress = associationPeerAddress association
    socket = associationSocket association
    tsn = 0

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
                    withAssociation socket tag toProcess
  where
    processChunks association toProcess = foldM (handleChunk socket) association toProcess
    withAssociation ConnectSocket{} _ chunks = do
        assoc <- takeMVar $ association socket
        assoc' <- processChunks assoc chunks
        putMVar (association socket) assoc'
    withAssociation ListenSocket{} tag chunks = do
        assocs <- takeMVar (associations socket)
        case Map.lookup tag assocs of
            Just assoc -> do
                assoc' <- processChunks assoc chunks
                putMVar (associations socket) (Map.insert tag assoc' assocs)
            Nothing -> putMVar (associations socket) assocs

handleInitAck :: Association -> Init -> IO(Association)
handleInitAck a@Association{..} initAck = do
    socketSendMessage associationSocket (ipAddress associationPeerAddress, portNumber associationPeerAddress) cookieEcho
    return newAssociation
  where
    peerVT = initiateTag initAck
    newAssociation = a { associationPeerVT = peerVT, associationState = COOKIEECHOED}
    cookieEcho = makeCookieEcho newAssociation initAck

handleChunk :: Socket -> Association -> Chunk -> IO(Association)
handleChunk socket association chunk
    | t == initAckChunkType = handleInitAck association $ fromChunk chunk
    | t == payloadChunkType = handlePayload association $ fromChunk chunk
    | t == shutdownChunkType = handleShutdown association $ fromChunk chunk
    | t == cookieAckChunkType = handleCookieAck association $ fromChunk chunk
    | t == selectiveAckChunkType = handleSelectiveAck association $ fromChunk chunk
    | otherwise = return association --putStrLn $ "Got chunk:" ++ show chunk -- return() -- exception?
  where
    t = chunkType chunk

handleCookieAck :: Association -> CookieAck -> IO(Association)
handleCookieAck association@Association{..} initAck = do
    (eventhandler associationSocket) $ Established association
    return association

handleShutdown :: Association -> Shutdown -> IO(Association)
handleShutdown association@Association{..} chunk = do
    (eventhandler associationSocket) $ Closed association
    return association

handlePayload :: Association -> Payload -> IO(Association)
handlePayload association@Association{..} payload = do 
    acknowledge association payload
    (eventhandler associationSocket) $ Data association $ userData payload
    return association

acknowledge :: Association -> Payload -> IO()
acknowledge association@Association{..} Payload{..} = do
    socketSendMessage associationSocket (ipAddress associationPeerAddress, portNumber associationPeerAddress) message
    return ()
  where
    message = Message (makeHeader association 0) [toChunk sack]
    sack = SelectiveAck tsn 1 [] []

handleSelectiveAck :: Association -> SelectiveAck -> IO(Association)
handleSelectiveAck association@Association{..} SelectiveAck{..} = do 
    (eventhandler associationSocket) $ Sent association $ cumulativeTSNAck
    return association

handleInit :: Socket -> IpAddress -> Message -> IO()
handleInit ConnectSocket{} _ _ = return () -- throw away init's when we're not listening
handleInit socket@ListenSocket{} address message = do
    time <- getCurrentTime
    myVT <- randomIO :: IO Int
    myTSN <- randomIO :: IO Int
    let responseMessage = makeInitResponse address message secret time myVT myTSN
    socketSendMessage socket (address, portnum) responseMessage
  where
    secret = secretKey socket
    portnum = fromIntegral $ (sourcePortNumber.header) message

socketSendMessage :: Socket -> (IpAddress, NBSD.PortNumber) -> Message -> IO()
socketSendMessage socket address message = do
    --putStrLn $ "SendMessage: " ++ (show message) ++ "To: " ++ (show address)
    _ <- NSB.sendTo (underLyingSocket $ stack socket) messageBytes (sockAddr address)
    return ()
  where
    messageBytes = (BS.concat . BL.toChunks) $ serializeMessage message

{- The Cookie Echo is sent by connecting party, when the listener receives it
 - the cookie is verified, the connection association is registered and an
 - acknowledgement is sent to the connecting party.
 -}
handleCookieEcho :: Socket -> IpAddress -> Message -> IO()
handleCookieEcho ConnectSocket{} _ _ = return ()
handleCookieEcho socket@ListenSocket{} peerAddr message = do
    when validMac $ do
        assocs <- takeMVar $ associations socket
        timer <- startTimer 10
        let association = almostAssociation timer
        let newAssocs =  Map.insert (associationVT association) association assocs
        putMVar (associations socket) newAssocs
        -- We've established our end!
        (eventhandler socket) $ Established association
        socketSendMessage socket (peerAddr, fromIntegral.sourcePortNumber.header$message) $ cookieAckMessage association
        return ()
  where
    (validMac, almostAssociation) = validateMac socket peerAddr message
    cookieAckMessage association =  Message (makeHeader association 0) [toChunk CookieAck]
