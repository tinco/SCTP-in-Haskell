{-# LANGUAGE MultiParamTypeClasses #-}
module SCTP.Streams.Action where
import qualified Data.ByteString as BS
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified StreamIO as IO
import SCTP.Streams.Event
import SCTP.Streams.State
import Control.Monad
import Control.Concurrent
import System.CPUTime
import System.Random
import Data.Time.Clock

data Action = MakeUdpSocket NS.SockAddr | ListenOnSocket NS.Socket Int | StopStack
            | RandomInteger | FreePortNumber | GetTime
            | Delay Int Integer Event 
            | SendUdpMessage NS.Socket BS.ByteString NS.SockAddr 
            | MakeThread (IO.Handler Action Event State) State
            | Emit (IO.Eventer Event) Event
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

    handleIO _ (MakeThread handler state) = IO.handlerLoop state handler 

    handleIO _ (Emit eventer event) = eventer event 

    handleIO eventer Eventer = eventer $ GotEventer eventer

    handleIO eventer StopStack = do
                        putStrLn "Goodbye"
                        eventer Done

