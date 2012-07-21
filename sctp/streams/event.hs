module SCTP.Streams.Event where
import qualified StreamIO as IO
import qualified Network.Socket as NS
import qualified Data.ByteString as BS
import Data.Time.Clock

data Event = Begin | Done | End
           | MadeUdpSocket NS.Socket  
           | GotUdpMessage (BS.ByteString, NS.SockAddr)
           | GotRandomIntegers [Int]
           | GotPortNumber Int
           | Time UTCTime
           | SentMessage Int
           deriving (Eq, Show)

instance IO.Event Event where
  startEvent = Begin
