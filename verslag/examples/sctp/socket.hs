module Socket where
import qualified Network.Socket as NS
import qualified Data.ByteString as BS
import Types
import Data.Word

protocolNumber = 132 -- at least I think it is..

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
connect :: Socket -> SockAddr -> IO ()
connect sock remoteAddress = do
    NS.sendTo rawSock packed_init_chunk (sockAddr remoteAddress)
    where
        common_header = CommonHeader (portNumber . hostAddress $ sock)
            (portNumber remoteAddress) 0 0
        rawSock = underLyingSocket sock
        init_chunk = undefined
        packed_init_chunk = undefined

{- Accept incoming connections -}
accept :: Socket -> PacketHandler -> IO ()
accept sock handler =
    --
