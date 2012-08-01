module SCTP.Streams.Event where
import qualified StreamIO as IO
import qualified Network.Socket as NS
import qualified Data.ByteString as BS
import Data.Time.Clock
import Data.Time.LocalTime

data Event = Begin | Done | End
           | MadeUdpSocket NS.Socket  
           | GotUdpMessage (BS.ByteString, NS.SockAddr)
           | GotRandomInteger Int
           | GotRandomIntegers Int [Int]
           | GotPortNumber Int
           | Time UTCTime
           | SentMessage Int
           | GotEventer (IO.Eventer Event)
           | MadeThread (IO.Eventer Event)
           | SCTPListener NS.SockAddr (IO.Eventer Event)
           | SCTPConnecter NS.SockAddr (IO.Eventer Event)
           deriving (Eq)

instance IO.Event Event where
  startEvent = Begin
