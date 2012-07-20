{-# LANGUAGE RecordWildCards #-}
module SCTP.Streams where
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.UTF8 (fromString)
import qualified Network.BSD as NBSD
import SCTP.Types
import SCTP.Utils
--import SCTP.Socket.Types
import SCTP.Socket.Utils
import SCTP.Socket.Timer
import qualified Data.Map as Map
import System.Random
import Data.Time.Clock
import qualified StreamIO as IO

import SCTP.Streams.Event
import SCTP.Streams.Action
import SCTP.Streams.State

-- Registering:
main = IO.handlerLoop handler

handler :: IO.Handler Action Event State
handler startState startEvent = (Last, [StartStack, StopStack])
