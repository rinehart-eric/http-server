module Http.Response (
    encodeResponse,
    Response(..),
    StatusCode(..),
    )
where

import Data.Monoid
import Data.ByteString.Builder
import qualified Data.ByteString.Char8 as B
import qualified Data.Map.Strict as Map

encodeResponse :: Response -> B.ByteString
encodeResponse = toLazyByteString . renderRequest

renderResponse :: Response -> Builder
renderResponse req =

-- | HTTP request type
data StatusCode = 100 | 101 | 200 | 201 | 202 | 203 | 204 | 205 | 206 | 300
                | 301 | 302 | 303 | 304 | 305 | 306 | 307 | 400 | 401 | 402
                | 403 | 404 | 405 | 406 | 407 | 408 | 409 | 410 | 411 | 412
                | 413 | 414 | 415 | 416 | 417 | 500 | 501 | 502 | 503 | 504
                | 505

reasonPhrase :: StatusCode -> String
reasonPhrase 200 = "OK"
reasonPhrase 404 = "Not Found"

-- | HTTP response data
data Response = Response
    { responseVersion :: String,
      statusCode      :: StatusCode,
      responseHeaders :: Map.Map String String,
      responseBody    :: Maybe String
    } deriving (Show)
