import SCTP.Socket

main :: IO ()
main = do
    listenAddress <- testUdpAddress
    putStrLn "Starting on udp"
    sctp <- start_on_udp listenAddress
    putStrLn "Listening"
    listenSocket <- listen sctp listenAddress listenHandler
    putStrLn "Connecting"
    connectSocket <- connect sctp listenAddress connectHandler
    putStrLn "Waiting"
    getLine
    return ()
  where
    listenHandler e = return ()-- putStrLn "Listen had event"
    connectHandler e = return () --putStrLn "Connect had event"
