module SCTP.Streams.Types where
import Network.Socket (HostAddress, HostAddress6)
import qualified Network.Socket as NS
import qualified Network.BSD as NBSD
import qualified Data.Map as Map
import Control.Concurrent.MVar
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word
import SCTP.Streams.Event
import SCTP.Types
import Data.Time.Clock


data Socket =  
  -- Socket is an instance of SCTP
  ListenSocket {
      socketAssociations :: Map.Map VerificationTag Association,
      secretKey :: BS.ByteString,
      socketEventer :: (Event -> IO())
  } |
  ConnectSocket {
      socketAssociation :: Association,
      socketVerificationTag :: VerificationTag,
      socketState :: SocketState,
      peerAddress :: NS.SockAddr,
      socketAddress :: NS.SockAddr,
      socketEventer :: (Event -> IO()),
      socketInitHandlers :: [InitHandler]
  }

data InitHandler = InitHandler (Maybe UTCTime) (Maybe Int) (Maybe Int) Message
newInitHandler = InitHandler Nothing Nothing Nothing

data SocketState = CONNECTING | CONNECTED | CLOSED
data AssociationState = COOKIEWAIT | COOKIEECHOED | ESTABLISHED |
                        SHUTDOWNPENDING | SHUTDOWNSENT | SHUTDOWNRECEIVED |
                        SHUTDOWNACKSENT

-- Transmission Control Block
data Association = Association {
    associationPeerVT :: VerificationTag,
    associationVT :: VerificationTag,
    associationState :: AssociationState,
    associationPort :: PortNum,
    associationPeerAddress :: NS.SockAddr,
    associationSocket :: Socket,
    associationTimeOut :: Integer -- in milliseconds
}
