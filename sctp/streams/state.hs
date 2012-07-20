module SCTP.Streams.State where
import qualified StreamIO as IO

data State = First | Last deriving (Eq, Show)
instance IO.State State where
  startState = First
