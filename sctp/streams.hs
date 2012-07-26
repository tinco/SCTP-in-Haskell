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
import SCTP.Streams.Types

handler :: IO.Handler Action Event State
handler s@(Setup eventer address) Begin = (s, [MakeUdpSocket address])
handler s@(Setup eventer address) (MadeUdpSocket sock) =
    (Established eventer address sock Map.empty, [ListenOnSocket sock maxMessageSize])
handler s@(Established eventer listenAddress sock sockets) (GotUdpMessage (bytes,peerAddress)) =
    (Established eventer listenAddress sock sockets', actions)
  where
    message = deserializeMessage bytes
    valid = verifyChecksum message
    destination = (ipAddress listenAddress, destinationPortNumber $ header message)
    noop = (sockets,[])
    (sockets', actions) | valid =
        case Map.lookup destination sockets of
            Just socket -> handleMessage socket
            Nothing -> noop
                        | otherwise = noop
    handleMessage socket = (sockets'', actions)
      where
        (socket', actions) = socketAcceptMessage socket (ipAddress peerAddress) message
        sockets'' = Map.insert destination socket' sockets

handler s@(Established e l sock sockets) event =
    (Established e l sock (dispatch $ Map.toList sockets),actions)
  where
    dispatch [] = (sockets,[])
    dispatch ((k,socket):sockets') =
        case socketHandler socket event of
            Just socket' -> updateSocket socket'
            Nothing -> dispatch sockets'
    updateSocket socket = (sockets,actions)
      where
        sockets = (Map.insert k socket' sockets)
        (socket', actions) = updateHandlers socket

socketHandler socket (GotRandomInteger int)
    | null end = Nothing
    | otherwise = Just socket'
  where
    (begin, end) = break eligibleInitHandler (socketInitHandlers socket)
    eligibleInitHandler (InitHandler _ Nothing Nothing _) = True
    eligibleInitHandler (InitHandler _ (Just _) Nothing _) = True
    eligibleInitHandler (InitHandler _ (Just _) (Just _) _) = False
    updateInitHandler (InitHandler a Nothing Nothing b) = InitHandler a (Just int) Nothing b
    updateInitHandler (InitHandler a b Nothing c) = InitHandler a b (Just int) c
    socket' = socket {
        socketInitHandlers = (updateInitHandler $ head end) : begin ++ (tail end)
    }

socketHandler socket (Time time)
    | null end = Nothing
    | otherwise = Just socket'
  where
    (begin, end) = break eligibleInitHandler (socketInitHandlers socket)
    eligibleInitHandler (InitHandler Nothing _ _ _) = True
    eligibleInitHandler (InitHandler (Just _) _ _ _) = False
    updateInitHandler (InitHandler Nothing a b c) = InitHandler (Just time) a b c
    socket' = socket {
        socketInitHandlers = (updateInitHandler $ head end) : begin ++ (tail end)
    }

type HandleResult = (Socket, [Action])

socketAcceptMessage socket address message
    | tag == 0 = handleInit socket address message
    | otherwise = case getAssociation socket of
          Just association -> processChunks association
          Nothing -> (socket', actions)
  where
    tag = verificationTag $ header message
    allChunks@(firstChunk : restChunks) = chunks message
    firstIsCookie = chunkType firstChunk == cookieEchoChunkType

    toProcess  | firstIsCookie = restChunks
               | otherwise = allChunks

    (socket' , actions ) | firstIsCookie = handleCookieEcho socket address message
                         | otherwise = (socket, [])

    processChunks association = foldl (handleChunk association) (socket', actions) toProcess
    getAssociation ConnectSocket{} = Just $ socketAssociation socket
    getAssociation ListenSocket{} = Map.lookup tag (socketAssociations socket)

handleChunk association r chunk  
    | t == initAckChunkType = handler handleInitAck
    | t == payloadChunkType = handler handlePayload
    | t == shutdownChunkType = handler handleShutdown
    | t == cookieAckChunkType = handler handleCookieAck
    | t == selectiveAckChunkType = handler handleSelectiveAck
    | otherwise = r --putStrLn $ "Got chunk:" ++ show chunk -- return() -- exception?
  where
    t = chunkType chunk
    handler f = f association r $ fromChunk chunk

handleInit socket@ConnectSocket{} _ _ = (socket, [])
handleInit socket@ListenSocket{..} address message = (socket', actions)
  where
    actions = [RandomInteger, RandomInteger, GetTime]
    handler = newInitHandler message
    socket' = socket { 
        socketInitHandlers = handler : socketInitHandlers socket'
    }

handleCookieEcho socket address message = (socket,[])

handleInitAck :: Association -> HandleResult -> Init -> HandleResult
handleInitAck _ r _ = r

handlePayload :: Association -> HandleResult -> Payload -> HandleResult
handlePayload _ r _ = r 

handleShutdown :: Association -> HandleResult -> Shutdown -> HandleResult
handleShutdown _ r _ = r

handleCookieAck :: Association -> HandleResult -> CookieAck -> HandleResult
handleCookieAck _ r _ = r

handleSelectiveAck :: Association -> HandleResult -> SelectiveAck -> HandleResult
handleSelectiveAck _ r _ = r

startSCTP address eventer = [MakeThread handler state]
  where
    state = Setup eventer address

listen address eventer = [MakeUdpSocket address]

{- Het idee is dat er een StartStack action is die een nieuwe handler thread maakt
 - met in de state de eventer van calling thread. Er komt ook een Emit actie die
 - een eventer als parameter neemt en daar events heen kan sturen. -}
