module SCTP.Utils where
import Data.Time
import Data.Word

-- our epoch is saturday, 19th of May at 16:31:21 UTC
epoch :: UTCTime
epoch = UTCTime (ModifiedJulianDay 56066) (secondsToDiffTime 59481)

timestamp at =  format $ diffUTCTime at epoch
  where
    format t = fromIntegral $ round t
