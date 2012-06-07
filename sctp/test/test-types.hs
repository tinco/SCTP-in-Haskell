{-# LANGUAGE TemplateHaskell #-}
import SCTP.Types
import Test.QuickCheck
import Test.QuickCheck.All
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Control.Monad
import Data.Word

main = $quickCheckAll

instance Arbitrary Cookie where
    arbitrary = liftM Cookie arbitrary
                     `ap` arbitrary
                     `ap` arbitrary
                     `ap` arbitrary
                     `ap` arbitrary
                     `ap` arbitrary
                     `ap` (elements [macZeroes])

instance Arbitrary IpAddress where
    arbitrary = oneof [
        liftM IPv4 arbitrary,
        liftM IPv6 (liftM4 (,,,) arbitrary arbitrary arbitrary arbitrary)
        ]

data MacArguments = MacArguments Cookie VerificationTag IpAddress Word16 BS.ByteString deriving (Show)
instance Arbitrary MacArguments where
    arbitrary = liftM MacArguments arbitrary
                     `ap` arbitrary
                     `ap` arbitrary
                     `ap` arbitrary
                     `ap` (elements [zeroes 1,macZeroes, zeroes 10, zeroes 60])

instance Arbitrary Parameter where
    arbitrary = liftM3 Parameter (elements [cookieType]) (elements [fromIntegral cookieLength + 4]) (liftM serializeCookie arbitrary)

macZeroes = zeroes macLength
zeroes length = BS.pack $ take length $ repeat (fromIntegral 0)

prop_serializingCookies :: Cookie -> Bool 
prop_serializingCookies c = deserialized == c
  where
    serialized = serializeCookie c
    (deserialized, _) = deserializeCookie serialized

prop_serializedCookieLength :: Cookie -> Bool 
prop_serializedCookieLength c = BL.length serialized == fromIntegral cookieLength
  where
    serialized = serializeCookie c

prop_makeMacHasRightLength :: MacArguments -> Bool
prop_makeMacHasRightLength args = (BS.length mac) == macLength
  where
    MacArguments cookie vt addr port secret = args
    mac = makeMac cookie vt addr port secret

prop_serializingParameter :: Parameter -> Bool
prop_serializingParameter p = deserialized == p
  where
    serialized = serializeParameter p
    (deserialized, _) = deserializeParameter serialized

prop_serializingChunk :: Chunk -> Bool
prop_serializingChunk p = deserialized == p
  where
    serialized = serializeChunk p
    (deserialized, _) = deserializeChunk serialized

instance Arbitrary Chunk  where
    arbitrary = liftM toChunk (oneof [arbitrary :: Gen Init])

instance Arbitrary Init where
    arbitrary = liftM Init arbitrary
                     `ap` length
                     `ap` arbitrary
                     `ap` arbitrary
                     `ap` arbitrary
                     `ap` arbitrary
                     `ap` arbitrary
                     `ap` liftM (\p -> p : []) arbitrary
      where
        parameter = arbitrary
        pLength = liftM (fromIntegral.parameterLength) parameter
        fLength = elements [fromIntegral initFixedLength]
        length = liftM2 (+) fLength pLength
