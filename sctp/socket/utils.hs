module SCTP.Socket.Utils where
import SCTP.Types
import SCTP.Socket.Types
import SCTP.Utils
import Data.Word
import qualified Data.ByteString as BS
import Control.Monad
import Control.Concurrent
import qualified Network.Socket as NS
import qualified Network.BSD as NBSD
import qualified Data.Map as Map

{- Get the default local server address -}
localServerAddress = do
    host <- NBSD.getHostByName "localhost"
    return $ NBSD.hostAddress host

testUdpPort = 54312 :: NBSD.PortNumber
testUdpAddress port = do
    localhost <- localServerAddress -- default serveraddress for localhost
    return (NS.SockAddrInet port localhost)

makeHeader :: Association -> Word32 -> CommonHeader
makeHeader association check = CommonHeader {
    sourcePortNumber = associationPort association,
    destinationPortNumber = (fromIntegral.portNumber.associationPeerAddress) association,
    verificationTag = associationPeerVT association,
    checksum = check
}

makeCookieEcho association init =
    Message (makeHeader association check) [toChunk echo]
  where
    cookieParameter = head $ parameters init
    cookie = deserializeCookie $ parameterValue cookieParameter
    echo = CookieEcho {
        cookieEcho = parameterValue cookieParameter
    }
    check = 0

makeInitResponse address message secret time myVT myTSN =
    --trace ("Sending Cookie: " ++ (show signedCookie)) $
    Message newHeader [toChunk initAck]
  where
    portnum = destinationPortNumber mHeader
    initChunk = (fromChunk $ head $ chunks message) :: Init
    mHeader = header message
    now = timestamp time
    peerVT = initiateTag initChunk
    cookie = Cookie now peerVT
     (advertisedReceiverWindowCredit initChunk)
     (numberOfOutboundStreams initChunk)
     (numberOfInboundStreams initChunk)
     (fromIntegral myTSN)
     BS.empty

    signedCookie = cookie { mac = makeMac cookie (fromIntegral myVT) address portnum secret }

    newHeader = CommonHeader {
        sourcePortNumber = destinationPortNumber mHeader,
        destinationPortNumber = sourcePortNumber mHeader,
        verificationTag = peerVT,
        checksum = 0
    }

    initAck = Init {
        initType = initAckChunkType,
        initLength = sum $ map fromIntegral [initFixedLength, fromIntegral parameterFixedLength, cookieLength] ,
        initiateTag = fromIntegral myVT,
        advertisedReceiverWindowCredit = advertisedReceiverWindowCredit initChunk, -- TODO be smart
        numberOfOutboundStreams = 1,
        numberOfInboundStreams = 1,
        initialTSN  = fromIntegral myTSN,
        parameters = [Parameter cookieType (fromIntegral cookieLength + fromIntegral parameterFixedLength) (serializeCookie signedCookie)]
    }


