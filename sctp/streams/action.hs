{-# LANGUAGE MultiParamTypeClasses #-}
module SCTP.Streams.Action where
import qualified Network.Socket as NS
import qualified Network.Socket.ByteString as NSB
import qualified StreamIO as IO
import SCTP.Streams.Event
import Control.Monad
import Control.Concurrent
import System.CPUTime
import System.Random

data Action = MakeUdpSocket NS.SockAddr | ListenSocket NS.Socket Int | StopStack
            | RandomInteger Int | FreePortNumber 
            | Delay Int Integer Event deriving (Eq, Show)

instance IO.Action Action Event where
    stopAction = StopStack
    handleIO eventer (MakeUdpSocket address) = do
        sock <- NS.socket NS.AF_INET NS.Datagram NS.defaultProtocol
        NS.bindSocket sock address
        eventer $ MadeUdpSocket sock

    handleIO eventer (ListenSocket sock maxMessageSize) =
        forever $ liftM eventer $ liftM GotUdpMessage $ NSB.recvFrom sock maxMessageSize

    handleIO eventer (RandomInteger n) = do
        randoms <- replicateM n (randomIO :: IO Int)
        eventer $ GotRandomIntegers randoms

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

    handleIO eventer StopStack = do
                        putStrLn "Goodbye"
                        eventer Done

