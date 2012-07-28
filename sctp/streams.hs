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
    (Established e l sock sockets', actions)
  where
    (sockets', actions) = dispatch $ Map.toList sockets
    dispatch [] = (sockets,[])
    dispatch ((k,socket):sockets) =
        case socketHandler socket event of
            Just (socket', actions) -> (sockets', actions)
              where
                sockets' = Map.insert k socket' (Map.fromList sockets)
            Nothing -> dispatch sockets

socketHandler :: Socket -> Event -> Maybe HandleResult
socketHandler socket event = runHandlers (socketHandlers socket) []
  where
    runHandlers (h:hs) hs' =
        case h (socket {socketHandlers = []}) event of
            Just (socket', actions) -> Just (socket'', actions)
              where
                socket'' = socket' {socketHandlers = hs' ++ (socketHandlers socket') ++ hs}
            Nothing -> runHandler hs (hs'++ [h])
    runHandler [] _ = Nothing

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

socketSendMessage socket address message =
    SendUdpMessage raw messageBytes address 
  where
    messageBytes = (BS.concat . BL.toChunks) $ serializeMessage message
    raw = socketRaw socket

handleInit socket@ConnectSocket{} _ _ = (socket, [])
handleInit socket@ListenSocket{..} address message = (socket', actions)
  where
    actions = [RandomInteger, RandomInteger, GetTime]
    h (Nothing, Nothing, t) s (GotRandomInteger i) = handle s (Just i, Nothing, t)
    h (i, Nothing, t) s (GotRandomInteger j) = handle s (i, Just j, t)
    h (i, j, Nothing) s (Time t) = handle s (i,j,Just t)
    h _ _ _ = Nothing
    handle s (Just i, Just j, Just t) = Just (s,actions)
      where
        portnum = fromIntegral $ (sourcePortNumber.header) message
        message' = makeInitResponse address message secretKey t i j
        actions = [socketSendMessage s destination message']
        destination = sockAddr (address, portnum)
    handle s t = Just (s {socketHandlers = [h t]}, [])

    socket' = socket { 
        socketHandlers = (socketHandlers socket') ++ [h (Nothing, Nothing, Nothing) ]
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
