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

sockAddr addr@(SockAddr {addrFamily = NS.AF_INET}) =
    NS.SockAddrInet (NS.PortNum $ portNumber addr) (head $ ipAddress addr)

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
bindSocket sock address = do
    NS.bindSocket sock (sockAddr address)

{- Connect! -}
connect sock remoteAddress = do
    NS.sendTo rawSock packed_init_chunk (sockAddr remoteAddress)
    where
        common_header = CommonHeader (portNumber . hostAddress $ sock)
            (portNumber remoteAddress) 0 0
        rawSock = underLyingSocket sock
        init_chunk = undefined
        packed_init_chunk = undefined
