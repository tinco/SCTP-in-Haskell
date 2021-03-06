{-# LANGUAGE MultiParamTypeClasses, RecordWildCards #-}
module SCTP.Streams.Types where
import Network.Socket (HostAddress, HostAddress6)
import qualified Network.Socket as NS
import qualified Network.BSD as NBSD
import qualified Data.Map as Map
import qualified Network.Socket.ByteString as NSB
import qualified StreamIO as IO
import Control.Concurrent.MVar
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word
import SCTP.Streams.Event
import qualified StreamIO as IO
import Data.Time.Clock
import qualified Data.Map as Map
import Control.Monad
import Control.Concurrent
import System.CPUTime
import System.Random
import Data.Time.Clock
import SCTP.Types

data Action = MakeUdpSocket NS.SockAddr | ListenOnSocket NS.Socket Int | StopStack
            | RandomInteger | FreePortNumber | GetTime | RandomIntegers Int 
            | Delay Int Integer Event 
            | SendUdpMessage NS.Socket BS.ByteString NS.SockAddr 
            | MakeThread (IO.Handler Action Event State) State
            | Emit (IO.Eventer Event) Event
            | SCTPListen NS.SockAddr (IO.Eventer Event)
            | SCTPConnect NS.SockAddr (IO.Eventer Event)
            | Eventer
            deriving (Eq)

instance IO.Action Action Event where
    stopAction = StopStack
    handleIO eventer (MakeUdpSocket address) = do
        sock <- NS.socket NS.AF_INET NS.Datagram NS.defaultProtocol
        NS.bindSocket sock address
        eventer $ MadeUdpSocket sock

    handleIO eventer (ListenOnSocket sock maxMessageSize) =
        forever $ liftM eventer $ liftM GotUdpMessage $ NSB.recvFrom sock maxMessageSize

    handleIO eventer (SendUdpMessage sock bytes addr) = do
        n <- NSB.sendTo sock bytes addr
        eventer $ SentMessage n

    handleIO eventer (RandomInteger) = do
        randoms <- randomIO :: IO Int
        eventer $ GotRandomInteger randoms

    handleIO eventer (RandomIntegers n) = do
        randoms <- replicateM n (randomIO :: IO Int)
        eventer $ GotRandomIntegers n randoms

    handleIO eventer (FreePortNumber) =
        eventer $ GotPortNumber 642213 --TODO obtain portnumber

    handleIO eventer (Delay interval delay event) = do
        time <- getCPUTime
        let delayTime = (toMilliseconds time) + delay
        let timerLoop = do
            now' <- getCPUTime
            let now = toMilliseconds now'
            if now > delayTime
                then eventer event
                else do
                    threadDelay $ interval * 1000
                    timerLoop
        timerLoop
      where
        toMilliseconds time = time `div` (10^9)

    handleIO eventer GetTime = do
        time <- getCurrentTime
        eventer $ Time time

    handleIO eventer (MakeThread handler state) = do
        eventer' <- IO.handlerLoop state handler 
        eventer $ MadeThread eventer'

    handleIO _ (Emit eventer event) = eventer event 

    handleIO eventer Eventer = eventer $ GotEventer eventer

    handleIO eventer StopStack = do
                        putStrLn "Goodbye"
                        eventer Done

data EitherResultOrHandler = AResult HandleResult | AHandler (Event -> Maybe EitherResultOrHandler) 

data State = Setup NS.SockAddr
           | Established {
              sctpUnderAddress :: NS.SockAddr,
              sctpUnderLyingSocket :: NS.Socket,
              sctpSockets :: (Map.Map (IpAddress, PortNum) Socket),
              sctpHandlers :: [Event -> Maybe EitherResultOrHandler]
           }

instance Eq State where
  (==) (Setup a) (Setup b) = a == b

instance Show State where
  show (Setup a) = show a


data Socket =  
  -- Socket is an instance of SCTP
  ListenSocket {
      socketAssociations :: Map.Map VerificationTag Association,
      socketSecret :: BS.ByteString,
      socketEventer :: (Event -> IO()),
      socketAddress :: NS.SockAddr,
      socketRaw :: NS.Socket,
      socketHandlers :: [Socket -> Event -> Maybe HandleResult]
  } |
  ConnectSocket {
      socketAssociation :: Association,
      socketVerificationTag :: VerificationTag,
      socketState :: SocketState,
      peerAddress :: NS.SockAddr,
      socketAddress :: NS.SockAddr,
      socketEventer :: (Event -> IO()),
      socketRaw :: NS.Socket,
      socketHandlers :: [Socket -> Event -> Maybe HandleResult]
  }

type HandleResult = (Socket, [Action])

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
    associationTimeOut :: Integer -- in milliseconds
}

associationHeader a@Association{..} =
    CommonHeader associationPort ((fromIntegral.portNumber) associationPeerAddress) associationPeerVT
