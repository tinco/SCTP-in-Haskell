module SCTP.Types where
import Data.ByteString hiding (map, foldl)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.Binary.Put
import qualified Data.Binary.BitPut as BP
import Data.Binary.Get
import qualified Data.Binary.Strict.BitGet as BG
import Data.Word
import Data.Bits

-- Every SCTP message follows the following structure
data Message = Message {
    header :: CommonHeader,
    chunks :: [Chunk]
}

serializeMessage message =
    foldl BL.append headerbytes chunkbytes
  where
    headerbytes = serializeCommonHeader $ header message
    chunkbytes = map serializeChunk $ chunks message

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

payloadChunkType = 0 :: Word8

instance ChunkType Payload where
  fromChunk c = Payload reserved u b e dataLength tsn streamIdentifier
                  streamSequenceNumber payloadProtocolIdentifier userData
                where
                  (reserved, u, b, e) = parseFlags (flags c)
                  dataLength = chunkLength c - 16 -- userData is length - 16 long
                  (tsn, streamIdentifier, streamSequenceNumber, payloadProtocolIdentifier, userData) = deserializePayload (fromIntegral dataLength) (value c)

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
  userData <- getLazyByteString length
  return (tsn, streamIdentifier, streamSequenceNumber, payloadProtocolIdentifier, userData)

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
  initLength :: Word16,
  initiateTag :: Word32,
  advertisedReceiverWindowCredit :: Word32,
  numberOfOutboundStreams :: Word16,
  numberOfInboundStreams :: Word16,
  initialTSN  :: Word32,
  parameters :: [Parameter]
} deriving (Show, Eq)

initChunkType = 1 :: Word8

instance ChunkType Init where
  fromChunk c = Init initLength initiateTag advertisedReceiverWindowCredit
                  numberOfOutboundStreams numberOfInboundStreams initialTSN parameters
    where
      initLength = chunkLength c
      (initiateTag, advertisedReceiverWindowCredit, numberOfOutboundStreams,
        numberOfInboundStreams, initialTSN, parameters) = deserializeInit $ value c

  toChunk i =
    Chunk chunkType flags cLength value
    where
      chunkType = 1
      flags = 0
      cLength = initLength i
      value = runPut $ do
        putWord32be $ initiateTag i
        putWord32be $ advertisedReceiverWindowCredit i
        putWord16be $ numberOfOutboundStreams i
        putWord16be $ numberOfInboundStreams i
        putWord32be $ initialTSN i
        putLazyByteString $ foldl BL.append BL.empty $ map serializeParameter $ parameters i

deserializeInit = runGet $ do
  initiateTag <- getWord32be
  advertisedReceiverWindowCredit <- getWord32be
  numberOfOutboundStreams <- getWord16be
  numberOfInboundStreams <- getWord16be
  initialTSN <- getWord32be
  parameters <- getRemainingLazyByteString
  return (initiateTag, advertisedReceiverWindowCredit, numberOfOutboundStreams,
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

serializeParameter :: Parameter -> BL.ByteString
serializeParameter p = runPut $ do
    putWord16be (parameterType p)
    putWord16be (parameterLength p)
    putLazyByteString (parameterValue p)

deserializeParameter :: BL.ByteString -> (Parameter, BL.ByteString)
deserializeParameter = runGet $ do
    pType <- getWord16be
    pLength <- getWord16be
    value <- getLazyByteString (fromIntegral pLength)
    rest <- getRemainingLazyByteString
    return $ (Parameter pType pLength value, rest)

deserializeParameters :: BL.ByteString -> [Parameter]
deserializeParameters bytes = parameter : other_parameters
    where
        (parameter, rest) = deserializeParameter bytes
        other_parameters = if BL.null rest then [] else deserializeParameters rest

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

data Cookie = Cookie {
  cookieLength :: Word16,
  cookie :: BL.ByteString
} deriving (Show, Eq)

cookieChunkType = 10 :: Word8

instance ChunkType Cookie where
  fromChunk c = Cookie cookieLength cookie
    where
      cookieLength = chunkLength c
      cookie = value c

  toChunk i =
    Chunk (cookieChunkType) 0 (cookieLength i) (cookie i)

serializeCookie i = (serializeChunk . toChunk) i

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
