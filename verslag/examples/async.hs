-- Async Haskell:
main :: IO()
main
	do readFile "someFile.txt" handle_file

handle_file :: String -> IO()
handle_file contents
	do writeFile "someOtherFile.txt" (upperCase contents) done
	where
		upperCase s = s

handle_file :: Error -> IO()
handle_file (Error error)
	do writeFile "someOtherFile.txt" ("Error: " ++ error) done

done :: IO()
done
	return ()
