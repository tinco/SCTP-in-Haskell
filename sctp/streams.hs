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
handler s@(Setup address) Begin = (s, [MakeUdpSocket address])
handler s@(Setup address) (MadeUdpSocket sock) =
    (Established address sock Map.empty [], [ListenOnSocket sock maxMessageSize])
handler s@(Established listenAddress sock sockets hs) (GotUdpMessage (bytes,peerAddress)) =
    (Established listenAddress sock sockets' hs, actions)
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

handler s@(Established listenAddress sock sockets hs) (SCTPListener addr eventer) =
  (Established listenAddress sock sockets' hs,actions)
  where
    sockets' = Map.insert (ipAddress addr,fromIntegral $ portNumber addr) socket sockets
    socket =  ListenSocket Map.empty BS.empty eventer addr sock handlers
    handlers = [h]
    h s (GotRandomIntegers 4 values) = Just (s', [])
      where
        key = BS.pack $ map fromIntegral values
        s' = s {socketSecret = key}
    h s _ = Nothing
    actions = [RandomIntegers 4]

handler s@(Established listenAddress sock sockets hs) (SCTPConnecter addr eventer) =
  (Established listenAddress sock sockets handlers,actions)
  where
    actions = [RandomInteger, FreePortNumber]
    handlers = hs ++ [h (Nothing, Nothing)]
    h (Nothing, p) (GotRandomInteger vt) = handle (Just vt ,p)
    h (vt, Nothing) (GotPortNumber p) = handle (vt ,Just p)
    h _ _ = Nothing
    handle (Just vt', Just p') = Just (AResult (socket, actions))
      where
        vt = fromIntegral vt'
        p = fromIntegral p'
        socket = ConnectSocket association vt CONNECTING addr (saddr) eventer sock []
        association = Association vt 0 COOKIEWAIT p addr defaultRTOInitial
        saddr = sockAddr (ipAddress listenAddress, fromIntegral p)
        actions = []
    handle t = Just (AHandler (h t))

handler s@(Established l sock sockets handlers) event =
    (Established l sock sockets' handlers', actions)
  where
    (handlers', sockets', actions) = dispatch handlers []
    dispatch [] hs_ = dispatchMap hs_ $ Map.toList sockets
    dispatch (h:hs) hs_ = case h event of
                   Just (AResult (socket, actions)) -> (hs_ ++ hs, sockets', actions)
                     where
                       sockets' = Map.insert k socket sockets
                       k = (ipAddress $ socketAddress socket, fromIntegral $ portNumber $ socketAddress socket)
                   Just (AHandler handler) -> (handler:hs_ ++ hs, sockets, actions)
                   Nothing -> dispatch hs (h:hs_)
    dispatchMap handlers [] = (handlers,sockets, [])
    dispatchMap handlers ((k,socket):sockets) =
        case socketHandler socket event of
            Just (socket', actions) -> (handlers, sockets', actions)
              where
                sockets' = Map.insert k socket' (Map.fromList sockets)
            Nothing -> dispatchMap handlers sockets

startSCTP address = [MakeThread handler state]
  where
    state = Setup address

{- Het idee is dat er een StartStack action is die een nieuwe handler thread maakt
 - met in de state de eventer van calling thread. Er komt ook een Emit actie die
 - een eventer als parameter neemt en daar events heen kan sturen. -}

listen stack addr eventer = [Emit stack listenEvent]
  where
    listenEvent = SCTPListener addr eventer
connect stack addr eventer = [Emit stack connectEvent]
  where
    connectEvent = SCTPConnecter addr eventer

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
        message' = makeInitResponse address message socketSecret t i j
        actions = [socketSendMessage s destination message']
        destination = sockAddr (address, portnum)
    handle s t = Just (s {socketHandlers = [h t]}, [])

    socket' = socket { 
        socketHandlers = (socketHandlers) ++ [h (Nothing, Nothing, Nothing) ]
    }

handleCookieEcho socket@ConnectSocket{} _ _ = (socket,[])
handleCookieEcho socket@ListenSocket{..} peerAddr message 
    | not validMac = (socket,[])
    | validMac = (socket',actions)
  where
    (validMac, vt,vt',p,p') = validateMac_ (ipAddress socketAddress) socketSecret message
    actions = [socketSendMessage socket destination message']
    --TODO Timer, Emit Eventhandler
    socket' = socket {socketAssociations = assocs'}
    assocs' = Map.insert (associationVT association) association socketAssociations
    association = Association vt' vt ESTABLISHED p destination defaultRTOInitial
    destination = sockAddr (peerAddr, fromIntegral.sourcePortNumber.header$message) 
    header' = CommonHeader p p' vt' 0
    message' =  Message header' [toChunk CookieAck]

handleInitAck :: Association -> HandleResult -> Init -> HandleResult
handleInitAck association@Association{..} (socket, actions) initAck = (socket', actions')
  where
    actions' = actions ++ [socketSendMessage socket associationPeerAddress message]
    header = associationHeader association' 0
    message = Message header [toChunk cookieEcho]
    association' = association { associationPeerVT = peerVT, associationState = COOKIEECHOED}
    assocs' = Map.insert (associationVT) association' (socketAssociations socket)
    socket' = socket {socketAssociations = assocs'}
    cookieEcho = CookieEcho (parameterValue $ head $ parameters initAck)
    peerVT = initiateTag initAck

handlePayload :: Association -> HandleResult -> Payload -> HandleResult
handlePayload _ r _ = r 

handleShutdown :: Association -> HandleResult -> Shutdown -> HandleResult
handleShutdown _ r _ = r

handleCookieAck :: Association -> HandleResult -> CookieAck -> HandleResult
handleCookieAck _ r _ = r

handleSelectiveAck :: Association -> HandleResult -> SelectiveAck -> HandleResult
handleSelectiveAck _ r _ = r

