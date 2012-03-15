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

protocolNumber = 132 -- at least I think it is..
                     -- change this to non-standard to circumvent
                     -- OS limitations wrt capturing kernel protocols

maxMessageSize = 4096 -- RFC specifies minimum of 1500

data SCTP = MkSCTP {
    underLyingSocket :: NS.Socket,
    address :: IpAddress,
    instances :: MVar (Map.Map (IpAddress, PortNum) (Chan Message))
}

-- Transmission Control Block
data Socket = MkSocket {
    socketChannel :: Chan Message,
    associations :: MVar (Map.Map VerificationTag (Chan Message)),
    secretKey :: BS.ByteString
}

data Association = MkAssociation {
    associationChannel :: Chan Message
    -- Transmission Control Block
}


data IpAddress = IPv4 Word32 | IPv6 (Word32, Word32, Word32, Word32) 
    deriving (Show, Eq, Ord)

ipAddress :: NS.SockAddr -> IpAddress
ipAddress (NS.SockAddrInet port host) = IPv4 host
ipAddress (NS.SockAddrInet6 port flow host scope) = IPv6 host

-- TODO I think this is no longer necessary
data SockAddr = SockAddr {
    addrFamily :: NS.Family,
    portNumber :: Word16,
    hostAddress :: [Word32],
    flow :: Word32,
    scope :: Word32
}

{- Get the default local server address -}
localServerAddress = do
    host <- NBSD.getHostByName "localhost"
    return $ NBSD.hostAddress host

{- Convert NS.SockAddr to a SockAddr -}
fromSockAddr :: NS.SockAddr -> SockAddr
fromSockAddr (NS.SockAddrInet port host) =
    SockAddr family (fromIntegral port) hostAddress 0 0 -- no flow or scope
    where
        family = NS.AF_INET
        hostAddress = [host]

fromSockAddr (NS.SockAddrInet6 port flow host scope) =
    SockAddr family (fromIntegral port) hostAddress flow scope -- no flow or scope
    where
        family = NS.AF_INET6
        (a, b, c, d) = host
        hostAddress = [a,b,c,d]


{- Create a NS.SockAddr from a SockAddr that is ipv4 -}
sockAddr addr@(SockAddr {addrFamily = NS.AF_INET}) =
    NS.SockAddrInet (NS.PortNum $ portNumber addr) (head $ hostAddress addr)

{- Create a NS.SockAddr from a SockAddr that is ipv6 -}
sockAddr addr@(SockAddr {addrFamily = NS.AF_INET6}) =
    NS.SockAddrInet6 (NS.PortNum $ portNumber addr) (flow addr) (ip6Addr) (scope addr)
    where
        ipAddr = hostAddress addr
        ip6Addr = (ipAddr !! 0, ipAddr !! 1, ipAddr !! 2, ipAddr !! 3)


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
    bytes <- NSB.recv (underLyingSocket stack) maxMessageSize
    let message = deserializeMessage bytes
    let h = header message
    let destination = (address stack, destinationPortNumber h)
    channels <- readMVar (instances stack)
    case Map.lookup destination channels of
        Just channel -> writeChan channel message
        Nothing -> return ()

{- Listen on Socket -}
listen :: SCTP -> SockAddr -> IO (Socket)
listen stack sockaddr = do
    channel <- newChan
    associations <- newMVar Map.empty
    keyValues <- replicateM 4 (randomIO :: IO(Int))
    let secretKey = BS.pack $ map fromIntegral keyValues
    let socket = MkSocket channel associations secretKey
    thread <- forkIO (listenSocketLoop socket)
    -- TODO it should be put in the stack to have messages dispatchd to the socketLoop
    return socket

    -- if it matches an existing stream, pass it to that stream
    -- if it is an init, generate a cookie
    -- it it is a cookie ack, establish a new connection
    -- TODO:
    -- connections and pending connections should be stored in Sockets
    -- so per stream configuration and variables can be read
    -- putTraceMsg $ "Header: " ++ (show header)
    -- putTraceMsg $ "Chunk: " ++ (show chunk)

listenSocketLoop socket = forever $ do
    message <- readChan $ socketChannel socket
    -- Drop packet if verifyChecksum fails
    if not $ verifyChecksum message then return()
        else do
        let tag = verificationTag $ header message
        if tag == 0 then -- reply with a cookie
            --reply socket message $ generateCookie message
            undefined
            else do
            -- extract chunks
            let (firstChunk : otherChunks) = chunks message
            if chunkType firstChunk == cookieChunkType then
                -- if first chunk is cookie echo, verify and make new association
                -- makeAssociation socket message
                undefined
                else do
                -- dispatch chunks to association
                associations <- readMVar (associations socket)
                case Map.lookup tag associations of
                    Just channel -> writeChan channel message
                    Nothing -> return()

    -- let handler =
    --         case () of _
    --                     | t == payloadChunkType -> handlePayload
    --                     | t == cookieChunkType -> handleCookie
    --         where t = toInteger $ chunkType chunk
    -- handler stack $ fromChunk chunk
    -- listenLoop stack

{- Connect to a remote socket at address -}
connect :: SCTP -> SockAddr -> IO (Socket)
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
    message <- readChan $ socketChannel socket
    let tag = verificationTag $ header message
    associations <- readMVar (associations socket)
    case Map.lookup tag associations of
        Just channel -> writeChan channel message
        Nothing -> return ()

accept :: Socket -> IO({- eehm?-})
accept socket =
    undefined
    -- block tot er een ESTABLISHED client is
    -- als er een ESTABLISHED client is, yield dan
    -- met de TCB


handleInit :: SCTP -> Init -> IO ()
handleInit stack init =
    undefined
    -- generate cookie
    -- init ack

handleCookie stack cookie =
    undefined
    -- build tcb
    -- cookie ack
    -- stream established

handlePayload stack payload =
    undefined

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
