module Socket where
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Network.BSD
import Control.Concurrent
import Debug.Trace
import Types
import Data.Word

protocolNumber = 132 -- at least I think it is..
					 -- change this to non-standard to circumvent
					 -- OS limitations wrt capturing kernel protocols

maxMessageSize = 4096 -- RFC specifies minimum of 1500

data Socket = MkSocket {
    underLyingSocket :: NS.Socket,
    sockAddress :: SockAddr,
    thread :: ThreadId
}

-- TODO I think this is no longer necessary
data SockAddr = SockAddr {
    addrFamily :: NS.Family,
    portNumber :: Word16,
    ipAddress :: [Word32],
    flow :: Word32,
    scope :: Word32
}

{- Get the default local server address -}
localServerAddress = do
    host <- getHostByName "localhost"
    return $ hostAddress host

{- Convert NS.SockAddr to a SockAddr -}
fromSockAddr :: NS.SockAddr -> SockAddr
fromSockAddr (NS.SockAddrInet port host) =
    SockAddr family (fromIntegral port) ipAddress 0 0 -- no flow or scope
    where
        family = NS.AF_INET
        ipAddress = [host]

fromSockAddr (NS.SockAddrInet6 port flow host scope) =
    SockAddr family (fromIntegral port) ipAddress flow scope -- no flow or scope
    where
        family = NS.AF_INET6
        (a, b, c, d) = host
        ipAddress = [a,b,c,d]


{- Create a NS.SockAddr from a SockAddr that is ipv4 -}
sockAddr addr@(SockAddr {addrFamily = NS.AF_INET}) =
    NS.SockAddrInet (NS.PortNum $ portNumber addr) (head $ ipAddress addr)

{- Create a NS.SockAddr from a SockAddr that is ipv6 -}
sockAddr addr@(SockAddr {addrFamily = NS.AF_INET6}) =
    NS.SockAddrInet6 (NS.PortNum $ portNumber addr) (flow addr) (ip6Addr) (scope addr)
    where
        ipAddr = ipAddress addr
        ip6Addr = (ipAddr !! 0, ipAddr !! 1, ipAddr !! 2, ipAddr !! 3)

{- Connect to a remote socket at address -}
connect :: Socket -> SockAddr -> IO (Int)
connect sock remoteAddress = do
    NS.sendTo rawSock packed_init_chunk (sockAddr remoteAddress)
    where
        common_header = CommonHeader (portNumber . sockAddress $ sock)
            (portNumber remoteAddress) 0 0
        rawSock = underLyingSocket sock
        init_chunk = undefined
        packed_init_chunk = undefined

{- Listen to incoming connections -}
listen :: SockAddr -> IO (Socket)
listen = listen_on_udp
    -- make the raw socket

{- Listen on Raw Socket -}
listen_with_socket :: NS.Socket -> IO (ThreadId)
listen_with_socket socket = do
    forkIO (listenLoop socket)

testUdpPort = 54312
{- Create an udp socket and use that as the raw socket backend -}
listen_on_udp :: SockAddr -> IO (Socket)
listen_on_udp address =
    do
        sock <- NS.socket NS.AF_INET NS.Datagram NS.defaultProtocol
        localhost <- localServerAddress -- default serveraddress for localhost
        NS.bindSocket sock (NS.SockAddrInet testUdpPort localhost)
        thread <- listen_with_socket sock
        let socket = MkSocket sock address thread
        return socket

    -- read data from socket
    -- parse the common header
    -- if it matches an existing stream, pass it to that stream
    -- if it is an init, generate a cookie
    -- it it is a cookie ack, establish a new connection
listenLoop :: NS.Socket -> IO ()
listenLoop socket = do
    bytes <- NSB.recv socket maxMessageSize
    let (header_bytes, chunk_bytes) = BS.splitAt (fromIntegral commonHeaderSize) bytes
    let header = deSerializeCommonHeader $ BL.fromChunks [header_bytes]
    let chunk = deSerializeChunk $ BL.fromChunks [chunk_bytes]
    putTraceMsg $ "Header: " ++ (show header)
    putTraceMsg $ "Chunk: " ++ (show chunk)
    listenLoop socket

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
