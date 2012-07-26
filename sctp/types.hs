module SCTP.Types where
import Data.ByteString hiding (map, foldl, take, length)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Binary.Put
import qualified Data.Binary.BitPut as BP
import Data.Binary.Get
import qualified Data.Binary.Strict.BitGet as BG
import Data.Word
import Data.Bits
import Data.HMAC
import Debug.Trace
import Control.Monad
import Network.Socket (HostAddress, HostAddress6)
import qualified Network.Socket as NS

data IpAddress = IPv4 Word32 | IPv6 (Word32, Word32, Word32, Word32)
    deriving (Show, Eq, Ord)

ipAddress :: NS.SockAddr -> IpAddress
ipAddress (NS.SockAddrInet port host) = IPv4 host
ipAddress (NS.SockAddrInet6 port flow host scope) = IPv6 host

portNumber :: NS.SockAddr -> NS.PortNumber
portNumber (NS.SockAddrInet port host) = port
portNumber (NS.SockAddrInet6 port flow host scope) = port

sockAddr :: (IpAddress, NS.PortNumber) -> NS.SockAddr
sockAddr (IPv4 host, port) = NS.SockAddrInet port host
sockAddr (IPv6 host, port) = NS.SockAddrInet6 port 0 host 0

protocolNumber :: Int
protocolNumber = 132 -- at least I think it is..
                     -- change this to non-standard to circumvent
                     -- OS limitations wrt capturing kernel protocols

maxMessageSize :: Int
maxMessageSize = 4096 -- RFC specifies minimum of 1500


{-
   RTO.Initial              - 3  seconds
   RTO.Min                  - 1  second
   RTO.Max                 -  60 seconds
   RTO.Alpha                - 1/8
   RTO.Beta                 - 1/4
   Valid.Cookie.Life        - 60  seconds
   Association.Max.Retrans  - 10 attempts
   Path.Max.Retrans         - 5  attempts (per destination address)
   Max.Init.Retransmits     - 8  attempts
   HB.interval              - 30 seconds
-}

defaultRTOInitial = 3000
defaultRTOMin = 1000
defaultRTOMax = 60000
defaultRTOAlpha = 125
defaultRTOBeta = 250
defaultValidCookieLife = 60000
defaultAssociationMaxRetrans = 10
defaultPathMaxRetrans = 5
defaultMaxInitRetransmits = 8
defaultHBInterval = 30

-- Every SCTP message follows the following structure
data Message = Message {
    header :: CommonHeader,
    chunks :: [Chunk]
} deriving (Show)

serializeMessage message =
    foldl BL.append headerbytes chunkbytes
  where
    headerbytes = serializeCommonHeader $ header message
    chunkbytes = map serializeChunk $ chunks message

deserializeMessage :: ByteString -> Message
deserializeMessage bytes = Message header chunks
    where
        (header_bytes, chunk_bytes) = BS.splitAt (fromIntegral commonHeaderSize) bytes
        header = deserializeCommonHeader $ BL.fromChunks [header_bytes]
        chunks = deserializeChunks $ BL.fromChunks [chunk_bytes]

deserializeChunks bytes = chunk : other_chunks
    where
        (chunk, rest) = deserializeChunk bytes
        other_chunks = if BL.null rest then [] else deserializeChunks rest

{-
                      SCTP Common Header Format

    0                   1                   2                   3
    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |     Source Port Number        |     Destination Port Number   |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                      Verification Tag                         |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                           Checksum                            |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-}
data CommonHeader = CommonHeader {
  sourcePortNumber :: PortNum,
  destinationPortNumber :: PortNum,
  verificationTag :: VerificationTag,
  checksum :: Word32
} deriving (Show, Eq)

serializeCommonHeader :: CommonHeader -> BL.ByteString
serializeCommonHeader h = runPut $ do
  putWord16be (sourcePortNumber h)
  putWord16be (destinationPortNumber h)
  putWord32be (verificationTag h)
  putWord32be (checksum h)

deserializeCommonHeader :: BL.ByteString -> CommonHeader
deserializeCommonHeader = runGet $ do
  sourcePortNumber <- getWord16be
  destinationPortNumber <- getWord16be
  verificationTag <- getWord32be
  checksum <- getWord32be

  return $ CommonHeader sourcePortNumber destinationPortNumber verificationTag checksum

commonHeaderSize = 12
type VerificationTag = Word32
type PortNum = Word16

{-
                                Chunk Format
        0                   1                   2                   3
        0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
       +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
       |   Chunk Type  | Chunk  Flags  |        Chunk Length           |
       +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
       \                                                               \
       /                          Chunk Value                          /
       \                                                               \
       +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-}
data Chunk = Chunk {
  chunkType :: Word8,
  flags :: Word8,
  chunkLength :: Word16,
  value :: BL.ByteString
} deriving (Show, Eq)

{-data AnyChunkType = forall t. ChunkType t => AnyChunkType t deriving (Show)-}
{-fromTypedChunk :: Word8 -> Chunk -> AnyChunkType-}
{-fromTypedChunk (initChunkType) c = fromChunk c :: Init-}
{-fromTypedChunk (initAckChunkType) c = fromChunk c :: Init-}
{-fromTypedChunk (cookieEchoChunkType) c = fromChunk c :: CookieEcho-}

instance ChunkType Chunk where
  toChunk = id
  fromChunk = id

class ChunkType t where
  toChunk :: t -> Chunk
  fromChunk :: Chunk -> t

serializeChunk :: Chunk -> BL.ByteString
serializeChunk h = runPut $ do
  putWord8 (chunkType h)
  putWord8 (flags h)
  putWord16be (chunkLength h)
  putLazyByteString (value h)

deserializeChunk :: BL.ByteString -> (Chunk, BL.ByteString)
deserializeChunk = runGet $ do
  chunkType <- getWord8
  flags <- getWord8
  chunkLength <- getWord16be
  value <- getLazyByteString (fromIntegral chunkLength)
  rest <- getRemainingLazyByteString
  return $ (Chunk chunkType flags chunkLength value, rest)

{-
                        Payload Data Format

    0                   1                   2                   3
    0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |   Type = 0    | Reserved|U|B|E|    Length                     |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                              TSN                              |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |      Stream Identifier S      |   Stream Sequence Number n    |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   |                  Payload Protocol Identifier                  |
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
   \                                                               \
   /                 User Data (seq n of Stream S)                 /
   \                                                               \
   +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-}
data Payload = Payload {
  reserved :: Word8, -- width is 5
  u :: Bool, b :: Bool, e :: Bool,
  dataLength :: Word16,
  tsn :: Word32,
  streamIdentifier :: Word16,
  streamSequenceNumber :: Word16,
  payloadProtocolIdentifier :: Word32,
  userData :: BL.ByteString
} deriving (Show, Eq)

fixedPayloadLength = 4 * 4 :: Word16

payloadPad :: BS.ByteString -> BS.ByteString
payloadPad userData = BS.concat [userData, (BS.pack $ take (fromIntegral $ newLength - oldLength) $ repeat 0)]
  where 
    newLength = (oldLength  + 3) `div` 4 * 4
    oldLength = BS.length userData

payloadPadLength :: Int -> Int
payloadPadLength length = length - (length + 3) `div` 4 * 4

payloadChunkType = 0 :: Word8
instance ChunkType Payload where
  fromChunk c = Payload reserved u b e dataLength tsn streamIdentifier
                  streamSequenceNumber payloadProtocolIdentifier userData
    where
      (reserved, u, b, e) = parseFlags (flags c)
      dataLength = chunkLength c - fixedPayloadLength
      (tsn, streamIdentifier, streamSequenceNumber,
       payloadProtocolIdentifier, userData) = 
          deserializePayload (fromIntegral dataLength) (value c)

  toChunk h =
      Chunk chunkType flags cLength value
    where
        chunkType = 0
        flags = BL.head . BP.runBitPut $ do
          BP.putNBits 5 (reserved h)
          BP.putBit (u h)
          BP.putBit (b h)
          BP.putBit (e h)
        cLength = dataLength h + 16
        value = runPut $ do
          putWord32be (tsn h)
          putWord16be (streamIdentifier h)
          putWord16be (streamSequenceNumber h)
          putWord32be (payloadProtocolIdentifier h)
          putLazyByteString (userData h)

parseFlags :: Word8 -> (Word8, Bool, Bool, Bool)
parseFlags flags = (reserved, u, b, e)
  where
    reserved = flags `shiftR` 3
    u = flags .&. 0x4 >= 1
    b = flags .&. 0x2 >= 1
    e = flags .&. 0x1 >= 1

serializePayload h = (serializeChunk . toChunk) h

deserializePayload length = runGet $ do
  tsn <- getWord32be
  streamIdentifier <- getWord16be
  streamSequenceNumber <- getWord16be
  payloadProtocolIdentifier <- getWord32be
  userData <- getLazyByteString $ fromIntegral length
  getLazyByteString $ fromIntegral $ length + payloadPadLength length -- padding
  return (tsn, streamIdentifier, streamSequenceNumber, payloadProtocolIdentifier, userData)

{-
                        Selective ACK chunk format
       0                   1                   2                   3
       0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      |   Type = 3    |Chunk  Flags   |      Chunk Length             |
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      |                      Cumulative TSN Ack                       |
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      |          Advertised Receiver Window Credit (a_rwnd)           |
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      | Number of Gap Ack Blocks = N  |  Number of Duplicate TSNs = X |
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      |  Gap Ack Block #1 Start       |   Gap Ack Block #1 End        |
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      /                                                               /
      \                              ...                              \
      /                                                               /
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      |   Gap Ack Block #N Start      |  Gap Ack Block #N End         |
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      |                       Duplicate TSN 1                         |
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      /                                                               /
      \                              ...                              \
      /                                                               /
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      |                       Duplicate TSN X                         |
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-}
data SelectiveAck = SelectiveAck {
  cumulativeTSNAck :: Word32,
  advertisedReceiverWindowCredit' :: Word32,
  gapAckBlocks :: [(Word16,Word16)],
  duplicateTSNs :: [Word32]
} deriving (Show, Eq)

fixedSelectiveAckLength = 4 * 4 :: Word16

selectiveAckChunkType = 3 :: Word8
instance ChunkType SelectiveAck where
  fromChunk c = SelectiveAck tsn a_rwnd gaps dups
    where
      deserializeSelectiveAck = runGet $ do
        tsn <- getWord32be
        a_rwnd <- getWord32be
        numGaps <- getWord16be
        numDups <- getWord16be
        let parseGap = do
            gapStart <- getWord16be
            gapEnd <- getWord16be
            return (gapStart, gapEnd)
        gaps <- replicateM (fromIntegral numGaps) parseGap
        dups <- replicateM (fromIntegral numDups) getWord32be
        return (tsn, a_rwnd, gaps, dups)
      (tsn,a_rwnd,gaps,dups) = deserializeSelectiveAck (value c)

  toChunk s =
      Chunk selectiveAckChunkType 0 cLength value
      where
        cLength = fixedSelectiveAckLength + numDups + numGaps
        numDups = fromIntegral $ length $ duplicateTSNs s
        numGaps = fromIntegral $ length $ gapAckBlocks s
        value = runPut $ do
          putWord32be $ cumulativeTSNAck s
          putWord32be $ advertisedReceiverWindowCredit' s
          putWord16be $ fromIntegral numGaps
          putWord16be $ fromIntegral numDups
          mapM_ (\(b,e) -> do
              putWord16be b
              putWord16be e
            ) $ gapAckBlocks s
          mapM_ putWord32be $ duplicateTSNs s

serializeSelectiveAck h = (serializeChunk . toChunk) h

{-
                              Init Chunk Format
        0                   1                   2                   3
        0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
       +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
       |   Type = 1    |  Chunk Flags  |      Chunk Length             |
       +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
       |                         Initiate Tag                          |
       +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
       |           Advertised Receiver Window Credit (a_rwnd)          |
       +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
       |  Number of Outbound Streams   |  Number of Inbound Streams    |
       +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
       |                          Initial TSN                          |
       +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
       \                                                               \
       /              Optional/Variable-Length Parameters              /
       \                                                               \
       +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-}
data Init = Init {
  initType :: Word8,
  initLength :: Word16,
  initiateTag :: Word32,
  advertisedReceiverWindowCredit :: Word32,
  numberOfOutboundStreams :: Word16,
  numberOfInboundStreams :: Word16,
  initialTSN  :: Word32,
  parameters :: [Parameter]
} deriving (Show, Eq)

initChunkType = 1 :: Word8
initAckChunkType = 2 :: Word8
initFixedLength = 5 * 4 :: Int

instance ChunkType Init where
  fromChunk c = Init initType initLength initiateTag advertisedReceiverWindowCredit
                  numberOfOutboundStreams numberOfInboundStreams initialTSN parameters
    where
      (initType, initLength, initiateTag, advertisedReceiverWindowCredit, numberOfOutboundStreams,
        numberOfInboundStreams, initialTSN, parameters) = deserializeInit $ value c

  toChunk i =
    Chunk chunkType flags cLength value
    where
      chunkType = (initType i)
      flags = 0
      cLength = initLength i
      value = runPut $ do
        putWord8 $ initType i
        putWord8 $ 0
        putWord16be $ initLength i
        putWord32be $ initiateTag i
        putWord32be $ advertisedReceiverWindowCredit i
        putWord16be $ numberOfOutboundStreams i
        putWord16be $ numberOfInboundStreams i
        putWord32be $ initialTSN i
        putLazyByteString $ foldl BL.append BL.empty $ map serializeParameter $ parameters i

deserializeInit = runGet $ do
  initType <- getWord8
  flags <- getWord8
  initLength <- getWord16be
  initiateTag <- getWord32be
  advertisedReceiverWindowCredit <- getWord32be
  numberOfOutboundStreams <- getWord16be
  numberOfInboundStreams <- getWord16be
  initialTSN <- getWord32be
  parameters <- getRemainingLazyByteString
  return (initType, initLength, initiateTag, advertisedReceiverWindowCredit, numberOfOutboundStreams,
      numberOfInboundStreams, initialTSN, deserializeParameters parameters)

serializeInit i = (serializeChunk . toChunk) i

{-                         Parameter
       0                   1                   2                   3
       0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      |          Parameter Type       |       Parameter Length        |
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      \                                                               \
      /                       Parameter Value                         /
      \                                                               \
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-}

data Parameter = Parameter {
  parameterType :: Word16,
  parameterLength :: Word16,
  parameterValue :: BL.ByteString
} deriving (Show, Eq)

parameterFixedLength = 4

serializeParameter :: Parameter -> BL.ByteString
serializeParameter p = runPut $ do
    putWord16be (parameterType p)
    putWord16be (parameterLength p)
    putLazyByteString (parameterValue p)

deserializeParameter :: BL.ByteString -> (Parameter, BL.ByteString)
deserializeParameter = runGet $ do
    pType <- getWord16be
    pLength <- getWord16be
    value <- getLazyByteString (fromIntegral pLength - parameterFixedLength)
    rest <- getRemainingLazyByteString
    return $ (Parameter pType pLength value, rest)

deserializeParameters :: BL.ByteString -> [Parameter]
deserializeParameters bytes = parameter : other_parameters
    where
        (parameter, rest) = deserializeParameter bytes
        other_parameters = if BL.null rest then [] else deserializeParameters rest

{-
  State Cookie
      Parameter Type Value: 7
      Parameter Length:  variable size, depending on Size of Cookie
      Parameter Value:

      I'm not sure what all belongs in the state cookie
    I'm guessing:
      - Cookie Creation Time (for stale checking)
      - Peer verification tag
      - Advertised window credit
      - Number of outbound streams
      - Maximum inbound streams
      - Initial TSN
      - MAC (HMAC) over:
        - My verification tag
        - Address of peer
    If we were to support multihoming the cookie would have to include every
    address supplied with the Init chunk. All these addresses would
    also need to be MAC'ed. This design makes no sense.
    We also don't support fast restarting (with tie-tags)
-}

cookieType = 7 :: Word16
cookieLength = 20 + macLength
macLength = 20

data Cookie = Cookie {
  cookieCreationTime :: Word32,
  peerVerificationTag :: VerificationTag,
  windowCredit :: Word32,
  outboundStreams :: Word16,
  maxInboundStreams :: Word16,
  myTSN :: Word32,
  mac :: BS.ByteString
} deriving (Show, Eq)

makeMac cookie myVerificationTag address portnum secret =
  --trace ("makeMac:" ++ (show (cookie, myVerificationTag, address, portnum, secret))) 
  --trace ("makeMac result:" ++ show mac) 
  mac
  where
    addrBytes = case address of
        IPv4 b -> [b]
        IPv6 (a,b,c,d) -> [a,b,c,d]

    bytes = runPut $ do
        putWord32be $ cookieCreationTime cookie
        putWord32be $ peerVerificationTag cookie
        putWord32be $ windowCredit cookie
        putWord16be $ outboundStreams cookie
        putWord16be $ maxInboundStreams cookie
        putWord32be $ myTSN cookie
        putWord32be myVerificationTag
        mapM_ putWord32be addrBytes
        putWord16be portnum
    mac = BS.pack $ take macLength $ hmac_sha1 (BS.unpack secret) $ BL.unpack bytes

serializeCookie c = runPut $ do
    putWord32be $ cookieCreationTime c
    putWord32be $ peerVerificationTag c
    putWord32be $ windowCredit c
    putWord16be $ outboundStreams c
    putWord16be $ maxInboundStreams c
    putWord32be $ myTSN c
    putByteString $ mac c

deserializeCookie :: BL.ByteString -> (Cookie, BL.ByteString)
deserializeCookie = runGet $ do
    cookieCreationTime <- getWord32be
    peerVerificationTag <- getWord32be
    windowCredit <- getWord32be
    outboundStreams <- getWord16be
    maxInboundStreams <- getWord16be
    myTSN <- getWord32be
    mac <- getByteString $ macLength
    rest <- getRemainingLazyByteString
    return (Cookie cookieCreationTime peerVerificationTag
             windowCredit outboundStreams maxInboundStreams myTSN mac, rest)

{-
                            Cookie Echo Chunk layout
        0                   1                   2                   3
        0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
       +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
       |   Type = 10   |Chunk  Flags   |         Length                |
       +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
       /                     Cookie                                    /
       \                                                               \
       +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-}

data CookieEcho = CookieEcho {
  cookieEcho :: BL.ByteString
} deriving (Show, Eq)

cookieEchoChunkType = 10 :: Word8
cookieEchoLength = 4 + (fromIntegral cookieLength) :: Word16

instance ChunkType CookieEcho where
  fromChunk c = CookieEcho cookieEcho
    where
      cookieEcho = value c

  toChunk i =
    Chunk (cookieEchoChunkType) 0 (cookieEchoLength) (cookieEcho i)

serializeCookieEcho i = (serializeChunk . toChunk) i

{-
                            Cookie Ack Layout
        0                   1                   2                   3
        0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
       +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
       |   Type = 11   |Chunk  Flags   |     Length = 4                |
       +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-}

data CookieAck = CookieAck deriving (Show, Eq)

cookieAckChunkType = 11 :: Word8

instance ChunkType CookieAck where
  fromChunk c = CookieAck

  toChunk i =
    Chunk cookieAckChunkType 0 4 BL.empty

serializeCookieAck i = (serializeChunk . toChunk) i

{-
                           Shutdown Layout
       0                   1                   2                   3
       0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      |   Type = 7    | Chunk  Flags  |      Length = 8               |
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
      |                      Cumulative TSN Ack                       |
      +-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+-+
-}
data Shutdown = Shutdown deriving (Show, Eq)

shutdownChunkType = 7 :: Word8

instance ChunkType Shutdown where
  fromChunk c = Shutdown

  toChunk i =
    Chunk shutdownChunkType 0 4 BL.empty -- TODO give TSN ack

serializeShutdown i = (serializeChunk . toChunk) i

-- main :: IO()
-- main = BL.putStr result
--  where
--   h = Chunk 1 1 3 (BL.pack [1,1,1])
--   result = serializeChunk(deserializeChunk (serializeChunk h))
