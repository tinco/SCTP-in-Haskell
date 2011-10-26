import qualified Data.ByteString.Lazy as BL
import Data.Binary.Put
import Data.Binary.BitPut
import Data.Word

data Header = Header {
  dataType :: Word8,
  reserved :: Word8, -- width is 5
  u :: Bool, b :: Bool, e :: Bool,
  dataLength :: Word16,
  tsn :: Word32,
  streamIdentifier :: Word16,
  streamSequenceNumber :: Word16
}

serializeHeader h = runPut $ do
  putWord8 (dataType h)
  putLazyByteString . runBitPut $ do
    putNBits 5 (reserved h)
    putBit (u h)
    putBit (b h)
    putBit (e h)
  putWord16be (dataLength h)
  putWord32be (tsn h)
  putWord16be (streamIdentifier h)
  putWord16be (streamSequenceNumber h)

main :: IO()
main = BL.putStr result
 where
  h = Header 255 0 True False True 65535 4294967295 65535 0
  result = serializeHeader h
