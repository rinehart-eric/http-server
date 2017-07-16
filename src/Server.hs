{-# LANGUAGE ScopedTypeVariables #-}

module Server(
    runServer,
    HandlerChain(..),
    RequestHandler(..),
    get,
    post
    )
where

import Control.Monad (when)
import Control.Monad.IO.Class
import qualified Data.ByteString as B
import Data.ByteString.Lazy (toStrict)
import qualified Data.Map as Map
import Data.Maybe
import Http.Request
import Http.Response
import Network.Simple.TCP
import Text.Regex.TDFA

runServer :: MonadIO m => String -> HandlerChain -> m ()
runServer port handlers = serve (Host "127.0.0.1") port (handleConnection handlers)

handleConnection :: HandlerChain -> (Socket, SockAddr) -> IO ()
handleConnection handlers (socket, addr) = do
        reqStr <- recv socket 65536
        let request = reqStr >>= parseRequest
            response = case request of
                Just r  -> handle handlers r
                Nothing -> emptyResponse 400
        when (isJust request) . putStrLn . ("Received request for "++) . requestPath $ fromJust request
        send socket . toStrict . encodeResponse $ response

type RequestHandler = (Request -> [String] -> Response)
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
handleType reqType pathRegex handler = (\req ->
        if requestType req == reqType
        then case matchFullPath pathRegex $ requestPath req of
                Just groups -> Just $ handler req groups
                Nothing     -> Nothing
        else Nothing)

matchFullPath :: String -> String -> Maybe [String]
matchFullPath regexString reqPath =
        let (b, _, a, g)::(String, String, String, [String]) = reqPath =~ regexString
        in if null b && null a
            then Just g
            else Nothing

notFoundHandler :: Request -> Response
notFoundHandler _ = emptyResponse 404

emptyResponse :: Int -> Response
emptyResponse s = Response {
        httpVersion = "1.1",
        statusCode = s,
        responseHeaders = Map.empty,
        responseBody = Nothing }
