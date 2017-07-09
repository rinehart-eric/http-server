module Main
where

import Data.ByteString.Char8 (pack)
import Data.Map as Map
import Http.Request
import Http.Response

requestMain = print . parseRequest . pack $ "GET /home HTTP/1.1\nUser-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\nHost: www.example.com\nAccept-Language: en, mi\n\nthis\nis\nthe\nbody"
responseMain = print . encodeResponse $ Response
    { httpVersion     = "1.1",
      statusCode      = 200,
      responseHeaders = Map.fromList [("User-Agent", "Me"), ("Location", "Here")],
      responseBody    = Just "This is the\nbody okay\nfriend"
    }

main = responseMain
