module Socket where
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
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
    hostAddress :: SockAddr
}

data SockAddr = SockAddr {
    addrFamily :: NS.Family,
    portNumber :: Word16,
    ipAddress :: [Word32],
    flow :: Word32,
    scope :: Word32
}

{- Create a NS.SockAddr from a SockAddr that is ipv4 -}
sockAddr addr@(SockAddr {addrFamily = NS.AF_INET}) =
    NS.SockAddrInet (NS.PortNum $ portNumber addr) (head $ ipAddress addr)

{- Create a NS.SockAddr from a SockAddr that is ipv6 -}
sockAddr addr@(SockAddr {addrFamily = NS.AF_INET6}) =
    NS.SockAddrInet6 (NS.PortNum $ portNumber addr) (flow addr) (ip6Addr) (scope addr)
    where
        ipAddr = ipAddress addr
        ip6Addr = (ipAddr !! 0, ipAddr !! 1, ipAddr !! 2, ipAddr !! 3)

{- Create an SCTP socket on a raw UNIX socket -}
socket :: SockAddr -> IO(Socket)
socket address = do
    sock <- NS.socket (addrFamily address) NS.Raw protocolNumber
    return $ MkSocket sock address

{- Attach an SCTP socket to a given address -}
bindSocket :: Socket -> SockAddr -> IO ()
bindSocket sock address = do
    NS.bindSocket (underLyingSocket sock) (sockAddr address)

{- Connect to a remote socket at address -}
connect :: Socket -> SockAddr -> IO (Int)
connect sock remoteAddress = do
    NS.sendTo rawSock packed_init_chunk (sockAddr remoteAddress)
    where
        common_header = CommonHeader (portNumber . hostAddress $ sock)
            (portNumber remoteAddress) 0 0
        rawSock = underLyingSocket sock
        init_chunk = undefined
        packed_init_chunk = undefined

{- Listen to incoming connections -}
start :: IO (ThreadId)
start =
    undefined
    -- make the raw socket

start_with_socket :: Socket -> IO (ThreadId)
start_with_socket sock = forkIO (socketLoop sock)

start_on_udp :: IO (ThreadId)
start_on_udp =
    undefined

    -- read data from socket
    -- parse the common header
    -- if it matches an existing stream, pass it to that stream
    -- if it is an init, generate a cookie
    -- it it is a cookie ack, establish a new connection
socketLoop :: Socket -> IO ()
socketLoop sock = do
    bytes <- NSB.recv socket maxMessageSize
    let (header_bytes, chunk_bytes) = BS.splitAt (fromIntegral commonHeaderSize) bytes
    let header = deSerializeCommonHeader $ BL.fromChunks [header_bytes]
    let chunk = deSerializeChunk $ BL.fromChunks [chunk_bytes]
    putTraceMsg $ "Header: " ++ (show header)
    putTraceMsg $ "Chunk: " ++ (show chunk)
    socketLoop sock
    where
        socket = underLyingSocket sock
