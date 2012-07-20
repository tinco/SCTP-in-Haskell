module SCTP.Streams.Event where
import qualified StreamIO as IO

data Event = Begin | Done | End deriving (Eq, Show)

instance IO.Event Event where
  startEvent = Begin
