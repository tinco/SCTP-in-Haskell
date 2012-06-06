{-# LANGUAGE TemplateHaskell #-}
import SCTP.Types
import Test.QuickCheck
import Test.QuickCheck.All
import qualified Data.ByteString as BS
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
                     `ap` (elements [macZeroes, zeroes 10, zeroes 60])


macZeroes = zeroes macLength
zeroes length = BS.pack $ take length $ repeat (fromIntegral 0)

prop_serializingCookies :: Cookie -> Bool 
prop_serializingCookies c = deserialized == c
  where
    serialized = serializeCookie c
    (deserialized, _) = deserializeCookie serialized

prop_makeMacHasRightLength :: MacArguments -> Bool
prop_makeMacHasRightLength args = (BS.length mac) == macLength
  where
    MacArguments cookie vt addr port secret = args
    mac = makeMac cookie vt addr port secret
