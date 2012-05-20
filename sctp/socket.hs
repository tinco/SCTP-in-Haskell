module SCTP.Socket where
import Network.Socket (HostAddress, HostAddress6)
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Network.BSD as NBSD
import Control.Monad
import Control.Concurrent
import Debug.Trace
import SCTP.Types
import Data.Word
import qualified Data.Map as Map
import Control.Concurrent.MVar
import System.Random
import SCTP.Utils

protocolNumber = 132 -- at least I think it is..
                     -- change this to non-standard to circumvent
                     -- OS limitations wrt capturing kernel protocols

maxMessageSize = 4096 -- RFC specifies minimum of 1500

data SCTP = MkSCTP {
    underLyingSocket :: NS.Socket,
    address :: IpAddress,
    instances :: MVar (Map.Map (IpAddress, PortNum) Socket)
}

-- Socket is an instance of SCTP
data Socket = MkSocket {
    associations :: MVar (Map.Map VerificationTag Association),
    secretKey :: BS.ByteString,
    stack :: SCTP
}

-- Transmission Control Block
data Association = MkAssociation {
    associationChannel :: Chan Message,
    peerVerificationTag :: VerificationTag,
    myVerificationTag :: VerificationTag,
    associationState :: MVar AssociationState
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
    (bytes, address) <- NSB.recvFrom (underLyingSocket stack) maxMessageSize
    let message = deserializeMessage bytes
    let h = header message
    let destination = (address stack, destinationPortNumber h)
    sockets <- readMVar (instances stack)
    case Map.lookup destination sockets of
        Just socket -> socketAcceptMessage socket (ipAddress address) message
        Nothing -> return ()

{- Listen on Socket -}
listen :: SCTP -> NS.SockAddr -> IO (Socket)
listen stack sockaddr = do
    associations <- newMVar Map.empty
    keyValues <- replicateM 4 (randomIO :: IO(Int))
    let secretKey = BS.pack $ map fromIntegral keyValues
    let socket = MkSocket associations secretKey stack
    registerSocket stack sockaddr socket
    return socket

registerSocket :: SCTP -> NS.SockAddr -> Socket -> IO()
registerSocket stack addr socket =
    -- TODO simply overrides existing sockets, is this what we want?
    modifyMVar_ (instances stack) (return . Map.insert (ipAddress addr, fromIntegral(portNumber addr)) socket)

    -- if it matches an existing stream, pass it to that stream
    -- if it is an init, generate a cookie
    -- it it is a cookie ack, establish a new connection
    -- TODO:
    -- connections and pending connections should be stored in Sockets
    -- so per stream configuration and variables can be read
    -- putTraceMsg $ "Header: " ++ (show header)
    -- putTraceMsg $ "Chunk: " ++ (show chunk)

socketAcceptMessage :: Socket -> Message -> IO()
socketAcceptMessage socket address message =
    -- Drop packet if verifyChecksum fails
    when (verifyChecksum message) $ do
        let tag = verificationTag $ header message
        if tag == 0 -- verification tag is 0, so message MUST be INIT
            then handleInit socket address message
            else do
                let allChunks@(firstChunk : restChunks) = chunks message
                let toProcess
                        | chunkType firstChunk == cookieChunkType = restChunks
                        | otherwise = allChunks
                when (chunkType firstChunk == cookieChunkType) $ handleCookieEcho message

                -- dispatch chunks to association
                associations <- readMVar (associations socket)
                case Map.lookup tag associations of
                    Just association -> mapM_ (handleChunk association) toProcess
                    Nothing -> return () -- handle OOTB cases

handleChunk association chunk
    | t  == payloadChunkType = handlePayload association $ fromChunk chunk
    | t  == shutdownChunkType = handleShutdown association $ fromChunk chunk
    | otherwise = return() -- exception?
  where
    t = chunkType chunk

handleShutdown :: Association -> Shutdown -> IO()
handleShutdown association chunk =
    undefined

handlePayload :: Association -> Payload -> IO()
handlePayload association chunk =
    undefined

handleInit :: Socket -> Message -> IO()
handleInit socket address message = do
    now <- timestamp getCurrentTime
    myVT <- fromIntegral (randomIO :: IO(Int))
    myTSN <- fromIntegral (randomIO :: IO(Int))
    let peerVT = verificationTag message
    let cookie = Cookie now peerVT
         (advertisedWindowCredit initChunk)
         (numberOfOutboundStreams initChunk)
         (numberOfInboundStreams initChunk)
         myTSN
         BS.empty

    let signedCookie = cookie { mac = makeMac cookie myVT address portnum secret }

    let newHeader = CommonHeader {
        sourcePortNumber = destinationPortNumber mHeader,
        destinationPortNumber = sourcePortNumber mHeader,
        verificationTag = peerVT,
        checksum = 0
    }

    let initAck = Init {
        initType = initAckChunkType,
        initLength = initFixedLength + cookieLength,
        initiateTag = myVT,
        adertisedReceiverWindowCredit = advertisedWindowCredit initChunk -- TODO be smart
        numberOfOutboundStreams = 1,
        numberOfInboundStreams = 1,
        initialTSN  = myTSN,
        parameters = [Parameter cookieType cookieLength (serializeCookie signedCookie)]
    }

    socketSendMessage socket (address, portnum) (Message newHeader [toChunk initAck])
  where
    mHeader = header message
    initChunk = (fromChunk $ head $ chunks message) :: Init
    secret = secretKey socket
    portnum = destinationPortNumber mHeader


socketSendMessage :: Socket -> IpAddress -> Message -> IO(Int)
socketSendMessage socket address message =
    NSB.sendTo (serializeMessage message) (sockAddr address) (underlyingSocket $ stack socket)

handleCookieEcho message =
    undefined


    -- let handler =
    --         case () of _
    --                     | t == payloadChunkType -> handlePayload
    --                     | t == cookieChunkType -> handleCookie
    --         where t = toInteger $ chunkType chunk
    -- handler stack $ fromChunk chunk
    -- listenLoop stack

{- Connect to a remote socket at address -}
connect :: SCTP -> NS.SockAddr -> IO (Socket)
connect stack remoteAddress =
    undefined
    -- maak een nieuwe socket aan
    -- registreer de nieuwe socket bij stack
    -- initieer de socket in de connect staat

    -- NS.sendTo rawSock packed_init_chunk (sockAddr remoteAddress)
    -- where
    --     common_header = CommonHeader (portNumber . sockAddress $ sock)
    --        (portNumber remoteAddress) 0 0
    --    rawSock = underLyingSocket sock
    --    init_chunk = undefined
    --    packed_init_chunk = undefined

connectSocketLoop socket = forever $ do
    --message <- readChan $ socketInputChannel socket
    --let tag = verificationTag $ header message
    --associations <- readMVar (associations socket)
    --case Map.lookup tag associations of
    --    Just channel -> writeChan channel message
    --    Nothing -> return ()
    undefined

accept :: Socket -> IO({- eehm?-})
accept socket =
    undefined
    -- block tot er een ESTABLISHED client is
    -- als er een ESTABLISHED client is, yield dan
    -- met de TCB


-- TODO implement
verifyChecksum message = True

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
