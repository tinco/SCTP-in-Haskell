module SCTP.Socket.Types where
import SCTP.Types
import Network.Socket (HostAddress, HostAddress6)
import qualified Network.Socket as NS
import qualified Network.BSD as NBSD
import qualified Data.Map as Map
import Control.Concurrent.MVar
import qualified Data.ByteString as BS

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

data Event = OtherEvent Message
           | Established Association
           | Closed Association

data SocketState = CONNECTING | CONNECTED | CLOSED

-- Transmission Control Block
data Association = MkAssociation {
    associationPeerVT :: VerificationTag,
    associationVT :: VerificationTag,
    associationState :: MVar AssociationState,
    associationPort :: PortNum,
    associationPeerAddress :: NS.SockAddr,
    associationSocket :: Socket
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
sockAddr (IPv6 host, port) = NS.SockAddrInet6 port 0 host 0

