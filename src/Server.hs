module Server(
    runServer
    )
where

import Control.Monad.IO.Class
import Network.Simple.TCP

runServer :: MonadIO m => String -> m ()
runServer port = serve (Host "127.0.0.1") port handleConnection

handleConnection :: (Socket, SockAddr) -> IO ()
handleConnection (socket, addr) = do
        putStrLn $ "TCP connection from " ++ show addr
