import Data.Bits
import Network.Socket
import Network.BSD
import Data.List

type HandlerFunc = SockAddr -> String -> IO ()

serveLog :: String -> handlerFunc -> IO()
serveLog port handlerfunc = withSocketsDo $
	do
		addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just port)
		let serveraddr = head addrinfos
		sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
		bindSocket sock (addrAddress serveraddr)

		-- loop forever
		procMessages sock
			where procMessages sock =
				do
					(msg, _, addr) <- recvFrom sock 1024
					handlerfunc addr msg
					procMessages sock

plainHandler :: HandlerFunc
plainHandler addr msg =
	putStrLn $ "From " ++ show addr ++ ": " ++ msg


-- wat ik zou willen?

main :: String :: (Events -> Events)-> IO()
main port handlers
	addrinfos <- getAddrInfo (Just (defaultHints {addrFlags = [AI_PASSIVE]})) Nothing (Just port)
	let serveraddr = head addrinfos
	sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
	bindSocket sock (addrAddress serveraddr)
	
	handle []
		where
			 handle events = do
				new_events <- events ++ eventsFromSocket sock -- made up eventsFromSocket returning events array
				newer_events <- new_events ++ eventsFromConsole
				handle (fold new_events handlers)

serveLog :: Events -> (Events. ([Handler], State)
servelog ((msg, _, addr):rest) =
	(Console, "From " ++ show addr + ": " ++ msg) : rest

servelog ((msg, Ok, addr):rest) =
	serveLogState2 rest
