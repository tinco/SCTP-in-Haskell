{-# LANGUAGE MultiParamTypeClasses #-}
module SCTP.Streams.Action where

import qualified StreamIO as IO
import SCTP.Streams.Event

data Action = MakeUdpSocket .. | ListenSocket .. | StopStack deriving (Eq, Show)

instance IO.Action Action Event where
    stopAction = StopStack
    handleIO eventer (MakeUdpSocket address) = do
                        sock <- NS.socket NS_AF_INET NS.Datagram NS.defaultProtocol
                        NS.bindSocket sock address
                        eventer $ MadeUdpSocket sock
    handleIO eventer (ListenSocket sock maxMessageSize) =
                        forever $ eventer $ GotUdpMessage $ NSB.recvFrom sock maxMessageSize
    handleIO eventer (RandomInteger n) = eventer $ GotRandomInteger $ replicateM n (randomIO :: IO Int)
    handleIO eventer (FreePortNumber) = eventer $ GotPortnumber $ return 642213 --TODO obtain portnumber
    handleIO eventer (Delay s a) = 
    handleIO eventer StopStack = do
                        putStrLn "Goodbye"
                        eventer Done

