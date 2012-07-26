module SCTP.Streams.State where
import qualified StreamIO as IO
import qualified Network.Socket as NS
import SCTP.Types
import SCTP.Streams.Event
import SCTP.Streams.Types
import qualified Data.Map as Map

data State = Setup (IO.Eventer Event) NS.SockAddr
           | Established {
              sctpEventer :: (IO.Eventer Event),
              sctpUnderAddress :: NS.SockAddr,
              sctpUnderLyingSocket :: NS.Socket,
              sctpSockets :: (Map.Map (IpAddress, PortNum) Socket)
           }

instance Eq State where
  (==) (Setup _ a) (Setup _ b) = a == b

instance Show State where
  show (Setup _ a) = show a
