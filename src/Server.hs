module Server(
    runServer,
    HandlerChain(..),
    get,
    post
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

runServer :: MonadIO m => String -> HandlerChain -> m ()
runServer port handlers = serve (Host "127.0.0.1") port (handleConnection handlers)

handleConnection :: HandlerChain -> (Socket, SockAddr) -> IO ()
handleConnection handlers (socket, addr) = do
        reqStr <- recv socket 65536
        let request = reqStr >>= parseRequest
            response = case request of
                Just r  -> handle handlers r
                Nothing -> emptyResponse 400
        send socket . toStrict . encodeResponse $ response

type RequestHandler = (Request -> Response)
type PatternHandler = (Request -> Maybe Response)
type HandlerChain = [PatternHandler]

handle :: HandlerChain -> Request -> Response
handle (h:hs) req = case h req of
        Just r  -> r
        Nothing -> handle hs req
handle []     req = notFoundHandler req

get :: String -> RequestHandler -> PatternHandler
get = handleType GET

post :: String -> RequestHandler -> PatternHandler
post = handleType POST

handleType :: RequestType -> String -> RequestHandler -> PatternHandler
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
