module Server(
    runServer
    )
where

import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Data.ByteString.Lazy (toStrict)
import qualified Data.Map as Map
import Http.Request
import Http.Response
import Network.Simple.TCP
import Text.Regex

runServer :: MonadIO m => String -> m ()
runServer port = serve (Host "127.0.0.1") port handleConnection

handleConnection :: (Socket, SockAddr) -> IO ()
handleConnection (socket, addr) = do
        reqStr <- recv socket 65536
        do case reqStr of
            Just r -> parseRequest r
            Nothing -> return ()
        send socket . toStrict . encodeResponse . notFoundHandler $ request

handleRecv :: Maybe B.ByteString -> IO (Maybe Request)
handleRecv =

type RequestHandler = (RequestType, Regex) -> (Request -> Response)
instance Monoid RequestHandler where
        mempty       = (_, _) -> notFoundHandler
        mappend r rs = (t, x) -> 

testHandler :: RequestType

get :: Regex -> (Request -> Response) -> RequestHandler
get r h = (GET, r, h)

post :: Regex -> (Request -> Response) -> RequestHandler
post r h = (POST, r, h)

notFoundHandler :: Request -> Response
notFoundHandler _ = Response {
        httpVersion = "1.1",
        statusCode = 404,
        responseHeaders = Map.empty,
        responseBody = Nothing }
