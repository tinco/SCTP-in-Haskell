{-# LANGUAGE MultiParamTypeClasses #-}
module SCTP.Streams.Action where
import qualified Data.ByteString as BS
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified StreamIO as IO
import SCTP.Streams.Event
import SCTP.Streams.State
