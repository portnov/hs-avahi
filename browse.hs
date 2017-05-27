
import Network.Avahi
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    [domain, service] -> do
      let query = BrowseQuery {
                    lookupProtocol = PROTO_UNSPEC,
                    lookupServiceName = service,
                    lookupDomain = domain,
                    lookupCallback = callback
                  }
      browse query
      putStrLn "hit enter when done"
      getLine
      return ()

    _ -> putStrLn "Synopsis: browse DOMAIN SERVICE\n\n\tFor example: browse local _printer._tcp"

callback :: Service -> IO ()
callback service = do
  print service

