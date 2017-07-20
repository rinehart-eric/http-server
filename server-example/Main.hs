module Main
where

import Data.ByteString.Char8 (pack)
import Data.List (intercalate)
import Data.Map as Map
import Http.Request
import Http.Response
import Server

requestMain = print . parseRequest . pack $ "GET /home HTTP/1.1\r\nUser-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\r\nHost: www.example.com\r\nAccept-Language: en, mi\r\n\r\nthis\nis\nthe\nbody"
responseMain = print . encodeResponse $ Response
    { httpVersion     = "1.1",
      statusCode      = 200,
      responseHeaders = Map.fromList [("User-Agent", "Me"), ("Location", "Here")],
      responseBody    = Just "This is the\nbody okay\nfriend"
    }
serverMain = runServer "8000" defaultHandler

main = serverMain

defaultHandler :: HandlerChain
defaultHandler = [
        get "/(.+)/(.+)" simpleResponse,
        get "/(.+)" simpleResponse
        ]

simpleResponse :: RequestHandler
simpleResponse req groups = Response {
        httpVersion = "1.1",
        statusCode = 200,
        responseHeaders = Map.empty,
        responseBody = Just $ groupString groups }

groupString :: [String] -> String
groupString xs = "[" ++ intercalate ", " xs ++ "]"
