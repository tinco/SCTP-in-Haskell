import Socket

sendString :: String -> IO()
	do
		sock <- socket AF_INET
		bindSocket sock address
		

main =
	do
		sendString "Hello World!"
