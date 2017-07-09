module Main
where

import Data.ByteString.Char8 (pack)
import Http.Request

main = print . parseRequest . pack $ "GET /home HTTP/1.1\n"
