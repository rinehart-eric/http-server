module Main
where

import Data.ByteString.Char8 (pack)
import Http.Request

main = print . parseRequest . pack $ "GET /home HTTP/1.1\nUser-Agent: curl/7.16.3 libcurl/7.16.3 OpenSSL/0.9.7l zlib/1.2.3\nHost: www.example.com\nAccept-Language: en, mi\n\nthis\nis\nthe\nbody"
