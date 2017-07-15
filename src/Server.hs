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
        let request = reqStr >>= parseRequest
            response = case request of
                Just r -> notFoundHandler r
                Nothing -> emptyResponse 400
        send socket . toStrict . encodeResponse $ response

type RequestHandler = (Request -> Response)

get :: String -> RequestHandler -> (Request -> Maybe Response)
get = handleType GET

post :: String -> RequestHandler -> (Request -> Maybe Response)
post = handleType POST

handleType :: RequestType -> String -> RequestHandler -> (Request -> Maybe Response)
handleType reqType path handler = (\req ->
        if requestType req == reqType && requestPath req == path
        then Just $ handler req
        else Nothing)

notFoundHandler :: Request -> Response
notFoundHandler _ = emptyResponse 404

emptyResponse :: Int -> Response
emptyResponse s = Response {
        httpVersion = "1.1",
        statusCode = s,
        responseHeaders = Map.empty,
        responseBody = Nothing }
